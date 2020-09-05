module HexCalc.ParseTests

open FParsec
open Fuchu

let testStr str test =
    testCase str (fun() ->
        match CharParsers.run Parse.expr str with
        | Success(result, _, _) ->
            test result |> ignore
        | Failure(msg, _, _) ->
            AssertException msg |> raise
    )

let tests =
    [
        let inline num nbase value =
            Integer { Base = nbase; Value = int64 value }

        "1 + 2", Add(num Base10 1, num Base10 2)
        "0xFFFF", num Base16 0xFFFF
    ]
    |> List.map (fun (str, expected) ->
        fun result ->
            Assert.Equal("equal expressions", expected, result)
        |> testStr str)
    |> testList "expression tests"
