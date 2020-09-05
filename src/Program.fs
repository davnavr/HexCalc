module HexCalc.Program

open System
open FParsec

let calc str =
    match run Parse.expr str with
    | Success(result, _, _) ->
        Expr.eval result |> Result.Ok
    | Failure(msg, _, _) ->
        Result.Error msg

let readLine color () =
    Console.ForegroundColor <- color
    Console.ReadLine()

let private print (msg: obj) color =
    Console.ForegroundColor <- color
    Console.WriteLine msg

[<EntryPoint>]
let main _ =
    let readExpr = readLine ConsoleColor.Gray >> calc
    // TODO: Figure out how to allow user to exit.
    while true do
        match readExpr() with
        | Result.Ok value ->
            print value ConsoleColor.Yellow
        | Result.Error msg ->
            print msg ConsoleColor.Red
    0
