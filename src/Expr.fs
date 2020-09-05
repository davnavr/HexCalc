[<RequireQualifiedAccess>]
module HexCalc.Expr

let rec private evaluate cont: _ -> Result<_, _> = // TODO: Figure out if this function is tail recursive.
    let emap (map: Integer -> Integer) expr =
        evaluate (Result.map map) expr
    let ebind binder expr =
        evaluate (Result.bind binder) expr
    let oper e1 e2 op =
        ebind
            (fun num1 ->
                emap
                    (fun num2 ->
                        { Base =
                            max num1.Base num2.Base
                          Value =
                            op num1.Value num2.Value })
                    e2)
            e1
    function
    | Integer num -> cont num
    | Add (e1, e2) -> oper e1 e2 (+)
    | Subtract (e1, e2) -> oper e1 e2 (-)
    | Multiply (e1, e2) -> oper e1 e2 (*)
    | Divide (e1, e2) -> oper e1 e2 (/)
    | ConvertBase (nbase, expr) ->
        emap (fun num -> { num with Base = nbase }) expr

let eval = evaluate id
