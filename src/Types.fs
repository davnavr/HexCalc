namespace HexCalc

open System

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
        let inline print c =
            match this.Value.Sign with
            | -1 -> sprintf "-0%c%s" c
            | _ -> sprintf "0%c%s" c
        let value = abs this.Value
        match this.Base with
        | Base10 -> string this.Value
        | Base16 when this.Value = bigint.Zero -> "0x0"
        | Base16 ->
            value
                .ToString("X")
                .TrimStart '0'
            |> print 'x'
        | Base2 when this.Value = bigint.Zero -> "0b0"
        | Base2 ->
            let str =
                value.ToByteArray()
                |> Seq.map
                    (fun b -> Convert.ToString(b, 2).PadLeft(8, '0'))
                |> Seq.rev
                |> String.Concat
            str.TrimStart('0') |> print 'b'

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
