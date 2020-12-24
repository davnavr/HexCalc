module HexCalc.Test

open Fuchu

let private tests =
    [
        IntegerTests.tests
        EvalTests.tests
        ProgramTests.tests

        testCase "hexadecimal numbers are printed correctly" <| fun() ->
            let num =
                { Base = Base16
                  Value = bigint 0x3C00 }
            Assert.Equal("String representations of number should match", "0x3C00", num.ToString())
    ]
    |> testList "tests"

[<EntryPoint>]
let main args = defaultMain tests args
