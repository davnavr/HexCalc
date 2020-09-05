﻿module HexCalc.ExprTests

open Fuchu
open System

open HexCalc.ParseTests

let tests =
    [
        "5 + 10", Base10, 15L
        "0b1 + 2", Base2, 3L
        "0x2 * 5", Base16, 10L
        "0x7FFF_FFFF_FFFF_FFFF", Base16, Int64.MaxValue
        "0x7FFF_FFFF_FFFF_FFFF + 1", Base16, Int64.MinValue
    ]
    |> List.map (fun (str, expbase, expval) ->
        fun result ->
            let expected =
                { Base = expbase
                  Value = expval }
            Assert.Equal("correct value", expected, Expr.eval result)
        |> testStr str)
    |> testList "expression tests"
