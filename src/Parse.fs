[<RequireQualifiedAccess>]
module HexCalc.Parse

open System
open FParsec

let private exprRef = OperatorPrecedenceParser<Expression, unit, unit>()

let private integer: Parser<_, unit> =
    let bchar =
        [ '0'; '1' ]
        |> List.map pchar
        |> choice
        <?> "binary digit"
    let digits p =
        skipChar '_'
        <?> "digit separator"
        |> many1
        |> sepBy1 (many1Chars p)
        |>> String.Concat
    let number nbase nvalue =
        { Base = nbase; Value = nvalue}
    choiceL
        [
            skipStringCI "0b"
            |> attempt
            >>. digits bchar
            <?> "binary"
            |>> fun str ->
                let rec parseb pow num =
                    function
                    | 0 -> num
                    | i ->
                        let index = i - 1
                        let next =
                            match str.Chars index with
                            | '0' -> num
                            | _ -> num + pown 2L pow
                        parseb (pow + 1) next index
                parseb 0 0L str.Length |> number Base2

            skipStringCI "0x"
            |> attempt
            >>. digits hex
            <?> "hexadecimal"
            |>> fun str ->
                let rec parseh pow num =
                    function
                    | 0 -> num
                    | i ->
                        let index = i - 1
                        let n =
                            let char =
                                str.Chars index |> int64 ||| int64 ' '
                            let offset =
                                if char <= int64 '9' then int64 '0' else 87L
                            char - offset
                        parseh (pow + 1) (num + n * pown 16L pow) index
                parseh 0 0L str.Length |> number Base16

            digits digit
            <?> "decimal"
            |>> (Int64.Parse >> number Base10)
        ]
        "integer"

let private nexpr =
    spaces >>. exprRef.ExpressionParser <?> "expression"

let expr = nexpr .>> eof

do
    let inline operator op = op :> Operator<_,_,_>
    let infixOp c op prec =
        (c.ToString(), spaces, prec, Associativity.Left, fun e1 e2 -> op(e1, e2))
        |> InfixOperator<_,_,_>
        |> operator

    [
        infixOp '+' Add 1
        infixOp '-' Subtract 1
        infixOp '*' Multiply 2
        infixOp '/' Divide 2
    ]
    |> List.iter exprRef.AddOperator

    exprRef.TermParser <-
        [
            integer |>> Integer

            skipChar '('
            |> attempt
            >>. nexpr
            .>> skipChar ')'
            <?> "nested expression"
        ]
        |> choice
        .>> spaces
