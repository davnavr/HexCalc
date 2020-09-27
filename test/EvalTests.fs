module HexCalc.EvalTests

open System
open Fuchu

open FParsec

let testStr str test =
    testCase str (fun() ->
        match runParserOnString Eval.expr State.Default "" str with
        | Success(result, _, _) ->
            test result |> ignore
        | Failure(msg, _, _) ->
            AssertException msg |> raise
    )

let tests =
    [
        "5 + 10", Base10, 15I
        "0b1 + 2", Base2, 3I
        "0x2 * 5", Base10, 10I
        "0x7FFF_FFFF_FFFF_FFFF", Base16, bigint Int64.MaxValue
        "  0x4 /   0x02", Base16, 2I
        "1 + 2 * 5", Base10, 11I
        "2 - 3 - 4", Base10, -5I
        "3 * (5 + 0b0000_0001)", Base2, 18I
        "7 * ( 8  + 9    ) * 10", Base10, 1190I
        "(-7 * -7) + 0b0001_0010", Base2, 67I
        "0x0 + 7 * (5 + (3 * 4))", Base10, 119I
        "-10 + 7", Base10, -3I
        " 0b1100 & 0b0101", Base2, 4I
        "0b0110 | 0x1", Base2, 7I
        "0x3 ^ 0b1100", Base2, 15I
        "10 % 7", Base10, 3I
        "dec(0xA)", Base10, 10I
        "hex(15)", Base16, 15I
        "bin(5)", Base2, 5I
        "dec  ( 0xFE + 1)", Base10, 255I
        "pow (2, 16)", Base10, 65536I
        "pow (0x10, 2) ", Base16, 256I
        "abs(-9)", Base10, 9I
        "abs ( -0b101 + 15)", Base2, 10I
        "ans + 0b0001_1010", Base2, 26I
        "MyVariable - 9", Base10, -9I
    ]
    |> List.map (fun (str, expbase, expval) ->
        fun actual ->
            let expected =
                { Base = expbase
                  Value = expval }
            Assert.Equal("correct value", expected, actual)
        |> testStr str)
    |> testList "expression evaluation"
