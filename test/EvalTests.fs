module HexCalc.ExprTests

open Fuchu

open HexCalc.ParseTests

let tests =
    [
        // Resulting base of expression
        "0b1 + 2", Base2, Int32 3
        "0x2 * 5", Base16, Int32 10
        "4 * 0xFF", Base16, Int32 1020

        // Resulting type of expression
        "2L + 1", Base10, Int64 3L
        "(-7 * -7) + 0b0001_0010I", Base10, Big 67I

        // Operator precedence & associativity
        "0x1A4", Base16, Int32 0x1A4
        "9 + 10", Base10, Int32 19
        "2 - 3 - 4", Base10, Int32 -5
        "1 + 2 * 5", Base10, Int32 11

        // Maximum values
        "0x7FFF_FFFF_FFFF_FFFFL", Base16, Int64 System.Int64.MaxValue
    ]
    |> testResults Eval.expr
    |> testList "evaluation tests"
