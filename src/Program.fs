module HexCalc.Program

open System
open FParsec

let private help =
    let inline merge strs = String.Join('\n', value = strs)
    [|
        "Type an expression such as '1 + 2' to get started!"
        "Examples:"
        "- '0x1A + 0b1101 * (5 - 3)'"
        "- '0xFFFF_FFFF / 2'"
        "- '(-7 * -7) + 0b0001_0010'"
        "Commands:"
        "'help' - Displays this help message"
        "'clear' or 'cls' - Clears the screen"
        "'quit' or 'exit' - Quits the application"
    |]
    |> merge

let calc str =
    match run Parse.input str with
    | Success(result, _, _) ->
        match result with
        | Input.Expr ex ->
            Expr.eval ex |> Output.Result
        | Input.Help -> Output.Help
        | Input.Clear -> Output.Clear
        | Input.Quit -> Output.Quit
    | Failure(msg, _, _) ->
        Output.Error msg

let private readLine color () =
    Console.ForegroundColor <- color
    Console.ReadLine()

let private print (msg: obj) color =
    Console.ForegroundColor <- color
    Console.WriteLine msg

[<EntryPoint>]
let main _ =
    printfn "Type 'help' for help"
    let readExpr = readLine ConsoleColor.Gray >> calc
    let mutable cont = true
    while cont do
        match readExpr() with
        | Output.Result value ->
            print value ConsoleColor.Yellow
        | Output.Help ->
            print help ConsoleColor.White
        | Output.Clear ->
            Console.Clear()
        | Output.Error msg ->
            print msg ConsoleColor.Red
        | Output.Quit -> cont <- false
    0
