[<RequireQualifiedAccess>]
module HexCalc.Eval

open FParsec

let private exprRef = OperatorPrecedenceParser<Integer, unit, State>()

let private digits ibase (ds: char[]): Parser<_, State> =
    let baseval = bigint ds.Length
    let rec buildint num =
        function
        | [] -> num
        | [] :: tail -> buildint num tail
        | (digit: int :: tail1) :: tail2 ->
            let num' = num * baseval + bigint digit
            let tail' = tail1 :: tail2
            buildint num' tail'
    let digit =
        Array.mapi
            (fun i d ->
                string d
                |> pstringCI
                >>% i)
            ds
        |> choice
        <?> "digit"
    let sep =
        skipChar '_' <?> "separator" |> many1
    sepBy1
        (many1 digit)
        sep
    |>> (fun digits ->
        { Base = ibase
          Value = buildint bigint.Zero digits })

let private integer: Parser<_, State> =
    choiceL
        [
            let dec = [| '0'..'9' |]

            pstringCI "0b"
            |> attempt
            >>. digits Base2 [| '0'; '1' |]

            pstringCI "0x"
            |> attempt
            >>. digits Base16 (Array.append dec [| 'a'..'f' |])

            digits Base10 dec
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
    let help =
        spaces1 >>. restOfLine false |> opt
    [
        expr
        >>= fun e ->
            fun state ->
                { state with Answer = e }
            |> updateUserState
            >>% (Command.Eval e |> Input)

        command [ "clear"; "cls" ] (Input Command.Clear)
        command [ "quit"; "exit" ] Input.Quit

        skipString "help"
        >>. help
        |>> (Command.Help >> Input)
    ]
    |> choice
    .>> eof

let private mapval map i  =
    { i with Value = map i.Value }
let private prefixOp op c prec =
    PrefixOperator<_, _, _>(c, spaces, prec, true, mapval op) :> Operator<_, _, _>
let private infixOp op c prec =
    let op' i1 i2 =
        { Base = min i1.Base i2.Base
          Value = op i1.Value i2.Value }
    InfixOperator<_, _, _>(c, spaces, prec, Associativity.Left, op') :> Operator<_,_,_>

do
    seq {
        for op in Terms.operators do
            let eoper =
                match op.Operation with
                | Terms.InfixOp oper -> infixOp oper
                | Terms.PrefixOp oper -> prefixOp oper
            eoper op.Symbol op.Precedence
    }
    |> Seq.iter exprRef.AddOperator

    exprRef.TermParser <-
        [
            integer

            skipString "ans"
            >>. getUserState
            |>> fun state -> state.Answer

            skipChar '('
            |> attempt
            >>. expr
            .>> skipChar ')'
            <?> "nested expression"

            choice
                [
                    for f in Terms.functions do
                        skipString f.Name
                        |> attempt
                        >>. spaces
                        >>. skipChar '('
                        >>. sepBy1 expr (skipChar ',')
                        .>> skipChar ')'
                        >>= fun args ->
                            match (f.Body, args) with
                            | (Terms.Arity1(_, op), [ arg ]) ->
                                op arg |> preturn
                            | (Terms.Arity2(_, _, op), [ arg1; arg2 ]) ->
                                op arg1 arg2 |> preturn
                            | _ ->
                                let exp =
                                    match f.Body with
                                    | Terms.Arity1 _ -> 1
                                    | Terms.Arity2 _ -> 2
                                sprintf
                                    "The function '%s' expects %i arguments, but got %i"
                                    f.Name
                                    exp
                                    args.Length
                                |> fail
                ]
            <?> "function call"
        ]
        |> choice
        .>> spaces
