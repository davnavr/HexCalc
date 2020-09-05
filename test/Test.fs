module HexCalc.Test

open Fuchu

let tests =
    [
        ExprTests.tests
    ]
    |> testList "tests"

[<EntryPoint>]
let main args = defaultMain tests args
