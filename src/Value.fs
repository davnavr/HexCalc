namespace rec HexCalc

open System

[<CustomComparison; CustomEquality>]
type Value =
    | UInt8 of uint8
    | Int32 of int32 // NOTE: This should be the default integer type.
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Big of bigint // TODO: Use i or I has numeric suffix.

    member private this.Convert cu8 c32 cu32 c64 cu64 cbig =
        match this with
        | UInt8 ui8 -> cu8 ui8
        | Int32 i32 -> c32 i32
        | UInt32 ui32 -> cu32 ui32
        | Int64 i64 -> c64 i64
        | UInt64 ui64 -> cu64 ui64
        | Big value -> cbig value

    member this.BigValue =
        this.Convert
            (Array.singleton >> bigint)
            bigint
            bigint
            bigint
            bigint
            id

    override this.Equals obj =
        this.BigValue = (obj :?> Value).BigValue

    override this.GetHashCode() = this.BigValue.GetHashCode()

    interface IComparable<Value> with
        member this.CompareTo(other) = compare this.BigValue other.BigValue

    static member op_Explicit(num: Value) =
        num.Convert uint64 uint64 uint64 uint64 uint64 uint64
    static member op_Explicit(num: Value) =
        num.Convert int64 int64 int64 int64 int64 int64
    static member op_Explicit(num: Value) =
        num.Convert uint32 uint32 uint32 uint32 uint32 uint32
    static member op_Explicit(num: Value) =
        num.Convert int32 int32 int32 int32 int32 int32
    static member op_Explicit(num: Value) =
        num.Convert uint8 uint8 uint8 uint8 uint8 uint8

[<RequireQualifiedAccess>]
module Value =
    let oper ou8 o32 ou32 o64 ou64 obig val1 val2 =
        let op f convert result =
            convert val1
            |> f (convert val2)
            |> Result.map result
        match (val1, val2) with
        | (Big _, _)
        | (_, Big _) ->
            obig val1.BigValue val2.BigValue |> Big |> Ok
        // TODO: Error if one is UInt64 and one is Int64.
        | (UInt64 _, _)
        | (_, UInt64 _) -> op ou64 uint64 UInt64
        | (Int64 _, _)
        | (_, Int64 _) -> op o64 int64 Int64
        // TODO: Error if one is UInt32 and one is Int32
        | (UInt32 _, _)
        | (_, UInt32 _) -> op ou32 uint32 UInt32
        | (Int32 _, _)
        | (_, Int32 _) -> op o32 int32 Int32
        | (UInt8 _, _)
        | (_, UInt8 _) -> op ou8 uint8 UInt8
