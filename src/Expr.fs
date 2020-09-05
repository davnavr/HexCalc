[<RequireQualifiedAccess>]
module HexCalc.Expr

// TODO: Figure out if this function is tail recursive.
// TODO: Figure out how integer overflow will be handled.
// TODO: There must be a problem with the evaluation of expressions, since 1 + 2 + 3 parses correctly but evaluates to 3 instead of 6.
let rec private evaluate cont = 
    let oper e1 e2 op cont =
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
