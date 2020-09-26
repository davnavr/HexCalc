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

let private print color (msg: obj) =
    Console.ForegroundColor <- color
    Console.WriteLine msg

// TODO: Add tests that call this function, ensuring that 'ans' functions properly.
// TODO: Tests that call this function should test that it is tail recursive (does not throw an SO Exception).
let rec start inf outf (state: State) ins outs =
    let instr, ins' = inf ins
    let cont = start inf outf state ins'
    match run Eval.input instr with
    | Success(input, _, _) ->
        match input with
        | Input cmd ->
            let outs' =
                match cmd with
                | Command.Eval result ->
                    string result |> Output.Result
                | Command.Help None -> Output.Messages help
                | Command.Help (Some "all") ->
                    Terms.all
                    |> Map.toSeq
                    |> Seq.map (fst >> sprintf "- %s")
                    |> Output.Messages
                | Command.Help (Some term) ->
                    match Terms.search term with
                    | Result.Ok desc ->
                        Output.Messages desc
                    | Result.Error _ ->
                        sprintf "Unknown term '%s'" term |> Output.Error
                | Command.Clear -> Output.Clear
                |> outf outs
            cont outs'
        | Input.Quit -> (state, ins', outs)
    | Failure(msg, _, _) ->
        let outs' = Output.Error msg |> outf outs
        cont outs'

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
        State.Default
        true
        ()
    |> ignore
    0
