namespace HexCalc

[<StructuralComparison; StructuralEquality>]
type Base =
    | Base2
    | Base10
    | Base16

[<CustomComparison; CustomEquality>]
type Integer =
    { Base: Base
      Value: int64 }

    override this.ToString() =
        let neg = 
            if this.Value < 0L then " (negative)" else ""
        match this.Base with
        | Base10 -> string this.Value
        | Base2 ->
            let bits = System.Convert.ToString(this.Value, 2)
            sprintf "0b%s%s" bits neg
        | Base16 ->
            sprintf "0x%X%s" this.Value neg

    override this.Equals(other) =
        this.Value = (other :?> Integer).Value

    override this.GetHashCode() = this.Value.GetHashCode()

    interface System.IComparable with
        member this.CompareTo(other) =
            this.Value.CompareTo (other :?> Integer).Value

[<StructuralComparison; StructuralEquality>]
type Expression =
    | Integer of Integer
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Modulo of Expression * Expression
    | Negate of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Xor of Expression * Expression

[<RequireQualifiedAccess>]
type Input =
    | Expr of Expression
    | Help of term: string option
    | Clear
    | Quit

[<RequireQualifiedAccess>]
type Output =
    | Result of Integer
    | Error of msg: string
    | Messages of lines: seq<string>
    | Clear
    | Quit
