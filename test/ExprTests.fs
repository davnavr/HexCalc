module HexCalc.ExprTests

open Fuchu

let tests =
    [
        test "add" {
            let inline num value =
                Ok { Base = Base10; Value = int64 value }
            let result =
                Add(num 3 |> Integer, num 4 |> Integer) |> Expr.eval
            Assert.Equal("expr", num 7, result)
        }
    ]
    |> testList "expression tests"
