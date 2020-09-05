module HexCalc.Program

open FParsec

let calc str =
    match run Parse.expr str with
    | Success(result, _, _) ->
        Expr.eval result |> Result.Ok
    | Failure(msg, _, _) ->
        Result.Error msg

[<EntryPoint>]
let main _ =
    // TODO: Figure out how to allow user to exit.
    while true do
        match stdin.ReadLine() |> calc with
        | Result.Ok value -> stdout.WriteLine value
        | Result.Error msg -> stderr.WriteLine msg
    0
