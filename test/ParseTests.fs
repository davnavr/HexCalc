module HexCalc.ParseTests

open Fuchu

let testStr str p test =
    testCase str (fun() ->
        match Parse.runstr str p with
        | Ok result ->
            test result |> ignore
        | Error err ->
            string err |> AssertException |> raise
    )

let tests =
    [
        "0B1000_0110uy", Base2, UInt8 134uy

        "0b0101", Base2, Int32 5
        "1234", Base10, Int32 1234
        "0______0", Base10, Int32 0
        "0xFFFF", Base16, Int32 0xFFFF

        "5678u", Base10, UInt32 5678u
        "0xAB_CDU", Base16, UInt32 0xABCDu

        "0X1234_5678I", Base16, Big 305419896I
        
        "0XfFuy", Base16, UInt8 255uy
        "21474_83647", Base10, Int32 2147483647
        "4_294_967_295u", Base10, UInt32 4294967295u
        "0x7fff_ffff_fFff_ffffL", Base16, Int64 9223372036854775807L
        "0xFFFF_FFFF_FFFF_FFFFUL", Base16, UInt64 0xFFFF_FFFF_FFFF_FFFFUL
    ]
    |> List.map (fun (str, expbase, expval) ->
        fun actual ->
            Assert.Equal("equal values", expval, actual.Value)
            Assert.Equal("equal bases", expbase, actual.Base)
        |> testStr str Parse.integer)
    |> testList "parse tests"
