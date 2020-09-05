[<RequireQualifiedAccess>]
module HexCalc.Parse

open System
open FParsec

let private exprRef = OperatorPrecedenceParser<Expression, unit, unit>()

let private integer: Parser<_, unit> =
    let bchar =
        [ '0'; '1' ] |> List.map pchar |> choice
    let hchar = hex |>> Char.ToLower
    let digits p =
        skipChar '_'
        |> many
        |> sepBy1 p
    let number nbase nvalue =
        { Base = nbase; Value = nvalue}
    choiceL
        [
            skipStringCI "0b"
            |> attempt
            >>. digits bchar
            <?> "binary"
            |>> fun chars ->
                let rec inner pow num =
                    function
                    | [] -> num
                    | char :: tail ->
                        let next =
                            match char with
                            | '0' -> num
                            | _ -> num + pown 2L (pow + 1)
                        inner (pow + 1) next tail
                inner 0 0L chars |> number Base2

            skipStringCI "0x"
            |> attempt
            >>. digits hchar
            <?> "hexadecimal"
            |>> fun chars ->
                let rec inner pow num =
                    function
                    | [] -> num
                    | char :: tail ->
                        let n =
                            match char with
                            | '0' -> 0L
                            | '1' -> 1L
                            | '2' -> 2L
                            | '3' -> 3L
                            | '4' -> 4L
                            | '5' -> 5L
                            | '6' -> 6L
                            | '7' -> 7L
                            | '8' -> 8L
                            | '9' -> 9L
                            | _ -> int64 char - 87L
                        inner (pow * 16u) (num + n * int64 pow) tail
                inner 1u 0L chars |> number Base16

            digits digit
            <?> "decimal"
            |>> fun chars ->
                Array.ofList chars
                |> String
                |> Int64.Parse
                |> number Base10
        ]
        "integer"

let expr = exprRef.TermParser <?> "expression"

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
        ]
        |> choice
        .>> spaces
