module HexCalc.ProgramTests

open Fuchu

let tests =
    [
        let inline outint ibase value = Output.Result { Base = ibase; Value = value }
        let outdec = outint Base10
        let outhex = outint Base16

        [ "quit" ], List.empty
        [ "1 + 2"; "ans * 3"; "quit" ], [ outdec 3I; outdec 9I ]
        [ "17"; "clear"; "1 + ans"; "quit" ], [ outdec 17I; Output.Clear; outdec 18I ]
        [ "Hello"; "Hello = 5 * 2"; "Hello + 1"; "quit" ], [ outdec 0I; outdec 10I; outdec 11I ]
        [ "90+\t5"; "Thing = hex(ans) - 0x4"; "ans"; "quit" ], [ outdec 95I; outhex 91I; outhex 91I; ]
        [ "MyHexValue = 0x11"; "quit" ], [ outhex 17I ]

        let rep =
            [
                for _ = 1 to 10 do
                    yield "ans + 1"
                yield "quit"
            ]
        rep, List.map outdec [ 1I..10I ]

        // TODO: Supply a large number of inputs to see if the program loop is tail recursive and does not throw an SOE.
    ]
    |> List.map (fun (inputs, expected) ->
        fun() ->
            let (_, _, actrev) =
                Program.start
                    (fun (strs: _ list) -> strs.Head, strs.Tail)
                    (fun outputs' out -> out :: outputs')
                    inputs
                    List.empty
                    State.Default
            Assert.Equal("outputs", expected, List.rev actrev)
        |> testCase (string inputs))
    |> testList "program input"
