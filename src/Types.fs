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

    override this.ToString() = // TODO: Fix, negative digit is not shown for hex or binary numbers.
        match this.Base with
        | Base2 -> System.Convert.ToString(this.Value, 2) |> sprintf "0b%s"
        | Base10 -> string this.Value
        | Base16 -> sprintf "0x%X" this.Value

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
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | Negate of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Xor of Expression * Expression

[<RequireQualifiedAccess>]
type Input =
    | Expr of Expression
    | Help
    | Clear

[<RequireQualifiedAccess>]
type Output =
    | Result of Integer
    | Error of msg: string
    | Help
    | Clear
