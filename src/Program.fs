module HexCalc.Program

open System
open FParsec

let private help =
    [
        "Type an expression such as '1 + 2' to get started!"
        "Commands:"
        "'help' - Display this help message"
        "'help <term>' - Displays information about an operator, function, or feature"
        "'help all' - Displays all terms with a help message"
        "'clear' or 'cls' - Clears the screen"
        "'quit' or 'exit' - Quits the application"
    ]

let input str =
    match run Parse.input str with
    | Success(result, _, _) ->
        match result with
        | Input.Expr ex ->
            Expr.eval ex |> Output.Result
        | Input.Help None -> Output.Messages help
        | Input.Help (Some "all") ->
            Terms.all
            |> Map.toSeq
            |> Seq.map (fst >> sprintf "- %s")
            |> Output.Messages
        | Input.Help (Some term) ->
            match Terms.search term with
            | Result.Ok desc ->
                Output.Messages desc
            | Result.Error _ ->
                sprintf "Unknown term '%s'" term |> Output.Error
        | Input.Clear -> Output.Clear
        | Input.Quit -> Output.Quit
    | Failure(msg, _, _) ->
        Output.Error msg

let private readLine color () =
    Console.ForegroundColor <- color
    Console.Write "> "
    Console.ReadLine()

let private print color (msg: obj) =
    Console.ForegroundColor <- color
    Console.WriteLine msg

[<EntryPoint>]
let main _ =
    printfn "Type 'help' for help"
    let readExpr = readLine ConsoleColor.Gray >> input
    let mutable cont = true
    while cont do
        match readExpr() with
        | Output.Result value ->
            print ConsoleColor.Yellow value
        | Output.Messages msgs ->
            Seq.iter (print ConsoleColor.White) msgs
        | Output.Clear ->
            Console.Clear()
        | Output.Error msg ->
            print ConsoleColor.Red msg
        | Output.Quit -> cont <- false
    0
