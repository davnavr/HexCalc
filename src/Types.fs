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

    // TODO: When the value is a bigint, use ToByteArray(?isUnsigned = true) to maybe get bytes as hex or binary.
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
