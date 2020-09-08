module HexCalc.Terms

type Operation =
    | Infix of (Expression * Expression -> Expression)
    | Prefix of (Expression -> Expression)

type Operator =
    { Description: string
      Example: string * string
      Name: string
      Operation: Operation
      Precedence: int
      Symbol: string }

let operators =
    List.map
        (fun (symbol, op, prec, name, desc, example, result) ->
            { Description = desc
              Example = (example, result)
              Name = name
              Operation = op
              Precedence = prec
              Symbol = symbol })
        [
            "&", Infix And, 1, "bitwise AND", "returns a number whose bits are set to one when the corresponding bit in both of the inputs are set to one", "0b111 & 0b110", "0b110"
            "|", Infix Or, 1, "bitwise OR", "returns a number whose bits are set to one when when the corresponding bit in one of the two inputs is set to one", "0b1100 | 0b0110", "0b1110"
            "^", Infix Xor, 2, "bitwise XOR", "returns a number whose bets are set to one when the corresponding bit in both of the inputs are different from each other", "0b1100 ^ 0b1011", "0b111"
            "+", Infix Add, 3, "addition", "adds two numbers together and returns the sum", "1 + 2", "3"
            "-", Infix Sub, 3, "subtraction", "subtracts the right number from the left number and returns the difference", "0xA - 1", "9"
            "*", Infix Mul, 4, "multiplication", "multiplies two numbers together and returns the product", "3 * 4", "12"
            "/", Infix Div, 4, "division", "divides the left number by the right number and returns the quotient", "9 / 0b11", "3"
            "%", Infix Modulo, 4, "modulo", "divides the left number by the right number and returns the remainder", "11 % 3", "2"
            // TODO: How to handle Negate operator, since its symbol is also the subtraction symbol.
        ]

let terms =
    operators
    |> Seq.map (fun op ->
        op.Symbol,
        [
            sprintf "The %s operator, %s." op.Name op.Description
            "Example:"
            sprintf "> %s" (fst op.Example)
            snd op.Example
        ])
    |> Map.ofSeq

let search =
    function
    | Some "all" ->
        Map.toList terms
        |> List.map (fst >> sprintf "- %s")
        |> Ok
        |> Some
    | Some term ->
        match Map.tryFind term terms with
        | Some desc -> Ok desc |> Some
        | None -> Error term |> Some
    | None -> None
