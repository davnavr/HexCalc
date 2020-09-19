namespace HexCalc

[<StructuralComparison; StructuralEquality>]
type Base =
    | Base2
    | Base10
    | Base16

[<CustomComparison; CustomEquality>]
type Integer =
    { Base: Base
      Value: bigint }

    override this.ToString() =
        match this.Base with
        | Base10 -> string this.Value
        | _ -> invalidOp "TODO: Add support for binary and hexadecimal, taking to account the weird behavior when the value is negative."

    override this.Equals(other) =
        this.Value = (other :?> Integer).Value

    override this.GetHashCode() = this.Value.GetHashCode()

    interface System.IComparable with
        member this.CompareTo(other) =
            this.Value.CompareTo (other :?> Integer).Value

[<RequireQualifiedAccess>]
type Input =
    | Eval of result: Integer
    | Help of term: string option
    | Clear
    | Quit

[<RequireQualifiedAccess>]
type Output =
    | Result of string
    | Error of msg: string
    | Messages of lines: seq<string>
    | Clear
    | Quit
