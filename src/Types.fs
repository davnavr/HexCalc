namespace HexCalc

[<StructuralComparison; StructuralEquality>]
type Base =
    | Base2
    | Base10
    | Base16

type Integer =
    { Base: Base
      Value: int64 }

type Expression =
    | Integer of Result<Integer, string>
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | ConvertBase of Base * Expression
    // TODO: Add other operations
