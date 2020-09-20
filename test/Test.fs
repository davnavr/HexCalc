module HexCalc.Test

open Fuchu

let tests =
    [
        IntegerTests.tests
        EvalTests.tests
    ]
    |> testList "tests"

[<EntryPoint>]
let main args = defaultMain tests args
