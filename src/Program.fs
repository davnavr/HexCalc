module HexCalc.Program

open System

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
    Output.Error "TODO: Process input"

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
