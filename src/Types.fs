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

    static member Zero =
        { Base = Base10
          Value = bigint.Zero }

    override this.ToString() =
        match this.Base with
        | Base10 -> string this.Value
        | Base16 ->
            let print =
                match this.Value.Sign with
                | -1 -> sprintf "-0x%s"
                | _ -> sprintf "0x%s"
            bigint.Abs(this.Value)
                .ToString("X")
                .TrimStart('0')
            |> print
        | Base2 ->
            invalidOp "not implemented for binary"

    override this.Equals(other) =
        this.Value = (other :?> Integer).Value

    override this.GetHashCode() = this.Value.GetHashCode()

    interface System.IComparable with
        member this.CompareTo(other) =
            this.Value.CompareTo (other :?> Integer).Value

[<RequireQualifiedAccess>]
type Command =
    | Eval of result: Integer
    | Help of term: string option
    | Clear
    | ListVariables

[<StructuralComparison; StructuralEquality>]
type Input =
    | Input of Command
    | Quit

[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type Output =
    | Result of Integer
    | Error of msg: string
    | Messages of lines: string list
    | Clear
