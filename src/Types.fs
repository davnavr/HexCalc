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
        let bstr f pfx (value: bigint) =
            let sign, value' =
                match value.Sign with
                | -1 -> "-", -value
                | _ -> "", value
            value'.ToByteArray()
            |> Seq.map f
            |> Seq.rev
            |> String.concat ""
            |> sprintf "%s%s%s" sign pfx
        let str =
            match this.Base with
            | Base10 -> string
            | Base16 ->
                bstr (sprintf "%X") "0x"
            | Base2 ->
                bstr (fun b -> System.Convert.ToString(b, 2)) "0b"
        str this.Value

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
