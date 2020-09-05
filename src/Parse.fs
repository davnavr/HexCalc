[<RequireQualifiedAccess>]
module HexCalc.Parse

open System
open FParsec

let private exprRef = OperatorPrecedenceParser<Expression, unit, unit>()

// TODO: Fix, overflow exception is thrown when number is too big.
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

let expr =
    spaces >>. exprRef.ExpressionParser <?> "expression"

let input =
    let command names cmd =
        List.map
            (skipString >> attempt)
            names
        |> choice
        >>% cmd
    [
        expr |>> Input.Expr
        command [ "help" ] Input.Help
        command [ "clear"; "cls" ] Input.Clear
    ]
    |> choice
    .>> eof

do
    let inline operator op = op :> Operator<_,_,_>
    let prefixOp c op prec =
        (c.ToString(), spaces, prec, true, fun ex -> op ex)
        |> PrefixOperator<_, _, _>
        |> operator
    let infixOp c op prec =
        (c, spaces, prec, Associativity.Left, fun e1 e2 -> op(e1, e2))
        |> InfixOperator<_,_,_>
        |> operator

    [
        infixOp "&" And 1
        infixOp "|" Or 1
        infixOp "^" Xor 2
        infixOp "+" Add 3
        infixOp "-" Subtract 3
        infixOp "*" Multiply 4
        infixOp "/" Divide 4
        prefixOp '-' Negate 5
    ]
    |> List.iter exprRef.AddOperator

    exprRef.TermParser <-
        [
            integer |>> Integer

            skipChar '('
            |> attempt
            >>. expr
            .>> skipChar ')'
            <?> "nested expression"
        ]
        |> choice
        .>> spaces
