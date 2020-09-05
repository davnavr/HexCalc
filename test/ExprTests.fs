module HexCalc.ExprTests

open Fuchu

let tests =
    [
        test "add" {
            let num value =
                { Base = Base10; Value = Int32 value }
            let result =
                Add(num 3 |> Integer, num 4 |> Integer) |> Expr.eval
            Assert.Equal("expr", num 7, result)
        }
    ]
    |> testList "expression tests"
