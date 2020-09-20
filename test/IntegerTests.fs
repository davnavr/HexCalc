module HexCalc.IntegerTests

open Fuchu

let tests =
    [
        Base10, 5678I, "5678"
        Base10, -7I, "-7"
        Base10, bigint.Zero, "0"
        Base2, 5I, "0b101"
        Base2, -2I, "-0b10"
        Base2, bigint.Zero, "0b0"
        Base16, bigint.One, "0x1"
        Base16, bigint 0x45, "0x45"
        Base16, -10I, "-0xA"
        Base16, bigint.Zero, "0x0"
    ]
    |> List.map (fun(ibase, ival, exp) ->
        fun() ->
            let integer =
                { Base = ibase
                  Value = ival }
            Assert.Equal("string representations", exp, string integer)
        |> testCase exp)
    |> testList "integer to string"
