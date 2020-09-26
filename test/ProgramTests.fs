module HexCalc.ProgramTests

open Fuchu

let tests =
    [
        let outint value = Output.Result { Base = Base10; Value = value }

        [ "1 + 2"; "ans * 3"; "quit" ], [ outint 3I; outint 9I ]
        [ "17"; "clear"; "1 + ans"; "quit" ], [ outint 17I; Output.Clear; outint 18I ]
        [ "Hello"; "Hello = 5 * 2"; "Hello + 1"; "quit" ], [ outint 0I; outint 10I; outint 11I ]

        let rep =
            [
                for _ = 1 to 10 do
                    yield "ans + 1"
                yield "quit"
            ]
        rep, List.map outint [ 1I..10I ]

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
