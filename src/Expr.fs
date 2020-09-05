[<RequireQualifiedAccess>]
module HexCalc.Expr

let rec private oper i1 i2 ouint8 oint8 oint32 =
    match (i1, i2) with
    | (UInt8 b1, UInt8 b2) -> ouint8 b1 b2 |> UInt8
    | (Int8 sb1, Int8 sb2) -> oint8 sb1 sb2 |> Int8
    | (Int32 i1, Int32 i2) -> oint32 i1 i2 |> Int32

let rec private evaluate cont: _ -> Integer =
    function
    | Integer num -> cont num
    | Add (e1, e2) ->
        evaluate
            (fun num1 ->
                evaluate
                    (fun num2 ->
                        { Base =
                            max num1.Base num2.Base
                          Value =
                            oper
                                num1.Value
                                num2.Value
                                (+)
                                (+)
                                (+) })
                    e2)
            e1

let eval = evaluate id
