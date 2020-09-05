﻿[<RequireQualifiedAccess>]
module HexCalc.Expr

// TODO: Make this function tail recursive.
let rec private evaluate cont = 
    let inline oper e1 e2 op cont =
        evaluate
            (fun num1 ->
                evaluate
                    (fun num2 ->
                        cont { num1 with Value = op num1.Value num2.Value })
                    e2)
            e1
    function
    | Integer num -> cont num
    | Add (e1, e2) -> oper e1 e2 (+) cont
    | Subtract (e1, e2) -> oper e1 e2 (-) cont
    | Multiply (e1, e2) -> oper e1 e2 (*) cont
    | Divide (e1, e2) -> oper e1 e2 (/) cont

let eval = evaluate id
