[<RequireQualifiedAccess>]
module HexCalc.Eval

open HexCalc.Parse
open HexCalc.Terms

let (expr: Parser<Integer>), private exprRef = forwarded()

let private term =
    spaces
    >>. choice
        [
            chr '('
            |> attempt
            >>. spaces
            >>. expr
            .>> spaces
            .>> chr ')'

            integer
        ]
    .>> spaces

let private infixops, private prefixops =
    let precsort =
        (fun (op: InfixOp) -> op.Precedence)
        |> List.groupBy 
        >> List.sortByDescending fst
    let ilist, plist =
        List.fold
            (fun (inf, pre) ->
                function
                | { Info = InfixOp op } -> (op :: inf, pre)
                | { Info = PrefixOp op } -> (inf, op :: pre))
            ([], [])
            operators
    precsort ilist, plist

exprRef :=
    // TODO: Add prefix operators here.
    term
    >>= fun t1 ->
        [
            term
            >>. fail (ErrorMessage "BAD")

            retn t1
        ]
        |> choice
