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
        "0xFFFF", Base16, Int32 0xFFFF
    ]
    |> List.map (fun (str, expbase, expval) ->
        fun actual ->
            Assert.Equal("equal values", expval, actual.Value)
            Assert.Equal("equal bases", expbase, actual.Base)
        |> testStr str Parse.integer)
    |> testList "parse tests"
