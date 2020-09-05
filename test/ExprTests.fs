module HexCalc.ExprTests

open System
open Fuchu

open HexCalc.ParseTests

let tests =
    [
        "5 + 10", Base10, 15L
        "0b1 + 2", Base2, 3L
        "0x2 * 5", Base16, 10L
        "0x7FFF_FFFF_FFFF_FFFF", Base16, Int64.MaxValue
        "0x7FFF_FFFF_FFFF_FFFF + 1", Base16, Int64.MinValue
        "  0x4 /   0x02", Base16, 2L
        "1 + 2 * 5", Base10, 11L
        "3 * (5 + 0b0000_0001)", Base10, 18L
        "7 * ( 8  + 9    ) * 10", Base10, 1190L
        "(-7 * -7) + 0b0001_0010", Base10, 67L
        "0x0 + 7 * (5 + (3 * 4))", Base16, 119L
        "-10 + 5", Base10, -5L

    ]
    |> List.map (fun (str, expbase, expval) ->
        fun result ->
            let expected =
                { Base = expbase
                  Value = expval }
            Assert.Equal("correct value", expected, Expr.eval result)
        |> testStr str)
    |> testList "expression tests"
