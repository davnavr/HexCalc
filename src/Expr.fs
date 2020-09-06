[<RequireQualifiedAccess>]
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
    | And (e1, e2) -> oper e1 e2 (&&&) cont
    | Or (e1, e2) -> oper e1 e2 (|||) cont
    | Xor (e1, e2) -> oper e1 e2 (^^^) cont
    | Add (e1, e2) -> oper e1 e2 (+) cont
    | Subtract (e1, e2) -> oper e1 e2 (-) cont
    | Multiply (e1, e2) -> oper e1 e2 (*) cont
    | Divide (e1, e2) -> oper e1 e2 (/) cont
    | Modulo (e1, e2) -> oper e1 e2 (%) cont
    | Negate expr ->
        evaluate
            (fun num -> cont { num with Value = -num.Value })
            expr

let eval = evaluate id
