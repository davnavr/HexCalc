﻿[<RequireQualifiedAccess>]
module HexCalc.Program

open System
open FParsec

let private help =
    [
        "Type an expression such as '1 + 2' to get started!"
        "Commands:"
        "'help' - Display this help message"
        "'help <term>' - Displays information about an operator, function, or keyword"
        "'help all' - Displays all terms with a help message"
        "'clear' or 'cls' - Clears the screen"
        "'listvars' - Lists all variables assigned with a non-zero value"
        "'quit' or 'exit' - Quits the application"
    ]

let private terms =
    Terms.all
    |> Map.toList
    |> List.map (fst >> sprintf "- %s")

let private print color (msg: obj) =
    Console.ForegroundColor <- color
    Console.WriteLine msg

let rec start inf outf ins outs (state: State) =
    let instr, ins' = inf ins
    let (state', msg) =
        try
            match runParserOnString Eval.input state "" instr with
            | Success(input, state', _) ->
                state',
                match input with
                | Input cmd ->
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
                    | Command.ListVariables ->
                        state'.Variables
                        |> Map.toList
                        |> List.map (fun (name, value) ->
                            sprintf "%s = %O" name value)
                        |> Output.Messages
                    |> Some
                | Input.Quit -> None
            | Failure(msg, _, _) ->
                state, Output.Error msg |> Some
        with
        | :? DivideByZeroException -> state, Output.Error "Cannot divide by zero" |> Some
    match msg with
    | Some output ->
        let outs' = outf outs output
        start inf outf ins' outs' state'
    | None ->
        state', ins', outs

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
