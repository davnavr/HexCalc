namespace HexCalc

type Value =
    | Int8 of int8
    | UInt8 of uint8
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64

[<StructuralComparison; StructuralEquality>]
type Base =
    | Base2
    | Base10
    | Base16

type Integer =
    { Base: Base
      Value: Value }

type Expression =
    | Integer of Integer
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | ConvertBase of Base * Expression
