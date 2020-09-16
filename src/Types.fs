namespace HexCalc

[<StructuralComparison; StructuralEquality>]
type Base =
    | Base2
    | Base10
    | Base16

[<CustomComparison; CustomEquality>]
type Integer =
    { Base: Base
      Value: Value }

    // TODO: When the value is a bigint, use ToByteArray(?isUnsigned = true) to maybe get bytes as hex or binary.
    //override this.ToString() =
    //    invalidOp "bad"

    override this.Equals(other) =
        this.Value = (other :?> Integer).Value

    override this.GetHashCode() = this.Value.GetHashCode()

    interface System.IComparable with
        member this.CompareTo(other) =
            compare this.Value (other :?> Integer).Value

[<RequireQualifiedAccess>]
type Input =
    | Expr of result: Integer
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
