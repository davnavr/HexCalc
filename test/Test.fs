module HexCalc.Test

open Fuchu

let private tests =
    [
        IntegerTests.tests
        EvalTests.tests
        ProgramTests.tests
    ]
    |> testList "tests"

[<EntryPoint>]
let main args = defaultMain tests args
