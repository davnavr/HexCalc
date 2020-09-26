[<RequireQualifiedAccess>]
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

let private terms =
    Terms.all
    |> Map.toList
    |> List.map (fst >> sprintf "- %s")

let private print color (msg: obj) =
    Console.ForegroundColor <- color
    Console.WriteLine msg

let rec start inf outf ins outs (state: State) = // TODO: Fix, ensure that this is tail recursive.
    let instr, ins' = inf ins
    let cont = outf outs >> start inf outf ins'
    match runParserOnString Eval.input state "" instr with
    | Success(input, state', _) ->
        match input with
        | Input cmd ->
            let output =
                match cmd with
                | Command.Eval result -> Output.Result result
                | Command.Help None -> Output.Messages help
                | Command.Help (Some "all") -> Output.Messages terms
                | Command.Help (Some term) ->
                    match Terms.search term with
                    | Result.Ok desc ->
                        Output.Messages desc
                    | Result.Error _ ->
                        sprintf "Unknown term '%s'" term |> Output.Error
                | Command.Clear -> Output.Clear
            cont output state'
        | Input.Quit -> state, ins', outs
    | Failure(msg, _, _) ->
        let err = Output.Error msg
        cont err state

[<EntryPoint>]
let main _ =
    let readin start =
        if start then
            printfn "Type 'help' for help"

        Console.ForegroundColor <- ConsoleColor.Gray
        Console.Write "> "
        Console.ReadLine(), false
    let writeout() =
        function
        | Output.Result value ->
            print ConsoleColor.Yellow value
        | Output.Messages msgs ->
            Seq.iter (print ConsoleColor.White) msgs
        | Output.Clear ->
            Console.Clear()
        | Output.Error msg ->
            print ConsoleColor.Red msg
    start
        readin
        writeout
        true
        ()
        State.Default
    |> ignore
    0
