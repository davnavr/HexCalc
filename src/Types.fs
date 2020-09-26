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
        let rec bstr f pfx (value: bigint): string =
            match value.Sign with
            | 0
            | 1 ->
                value.ToByteArray()
                |> Seq.map f
                |> Seq.rev
                |> String.concat ""
                |> sprintf "%s%s" pfx
            | _ ->
                -value |> bstr f pfx |> sprintf "-%s"
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

type Input =
    | Input of Command
    | Quit

[<RequireQualifiedAccess>]
type Output =
    | Result of string
    | Error of msg: string
    | Messages of lines: seq<string>
    | Clear

type State =
    { Answer: Integer }

    static member Default =
        { Answer = Integer.Zero }
