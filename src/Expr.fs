[<RequireQualifiedAccess>]
module HexCalc.Expr

// TODO: Figure out if this function is tail recursive.
// TODO: Figure out how integer overflow will be handled.
let rec private evaluate cont = 
    let oper e1 e2 op =
        evaluate
            (fun num1 ->
                evaluate
                    (fun num2 ->
                        { num1 with Value = op num1.Value num2.Value })
                    e2)
            e1
    function
    | Integer num -> cont num
    | Add (e1, e2) -> oper e1 e2 (+)
    | Subtract (e1, e2) -> oper e1 e2 (-)
    | Multiply (e1, e2) -> oper e1 e2 (*)
    | Divide (e1, e2) -> oper e1 e2 (/)

let eval = evaluate id
