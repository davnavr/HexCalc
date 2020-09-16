module HexCalc.ParseTests

open Fuchu

let testStr str p test =
    testCase str (fun() ->
        match Parse.runstr str p with
        | Ok result ->
            test result |> ignore
        | Error err ->
            string err |> AssertException |> raise
    )

let tests =
    [
        "0xFFFF", num Base16 0xFFFF
    ]
    |> List.map (fun (str, expected) ->
        fun result ->
            Assert.Equal("equal integers", expected, result)
        |> testStr str)
    |> testList "parse tests"
