module HexCalc.Terms

type Operation =
    | InfixOp of (Integer * Integer -> Integer)
    | PrefixOp of (Integer -> Integer)

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
            //"&", InfixOp And, 1, "bitwise AND", "returns a number whose bits are set to one when the corresponding bit in both of the inputs are set to one", "0b111 & 0b110", "0b110"
            //"|", InfixOp Or, 1, "bitwise OR", "returns a number whose bits are set to one when when the corresponding bit in one of the two inputs is set to one", "0b1100 | 0b0110", "0b1110"
            //"^", InfixOp Xor, 2, "bitwise XOR", "returns a number whose bets are set to one when the corresponding bit in both of the inputs are different from each other", "0b1100 ^ 0b1011", "0b111"
            //"+", InfixOp Add, 3, "addition", "adds two numbers together and returns the sum", "1 + 2", "3"
            //"-", InfixOp Sub, 3, "subtraction", "subtracts the right number from the left number and returns the difference", "0xA - 1", "9"
            //"*", InfixOp Mul, 4, "multiplication", "multiplies two numbers together and returns the product", "3 * 4", "12"
            //"/", InfixOp Div, 4, "division", "divides the left number by the right number and returns the quotient", "9 / 0b11", "3"
            //"%", InfixOp Modulo, 4, "modulo", "divides the left number by the right number and returns the remainder", "11 % 3", "2"
            //"-", PrefixOp Negate, 5, "negation", "returns the opposite of its operand, which is the operand multiplied by -1", "-5", "-5"
        ]

let all: Map<string, string list> =
    Map.empty
    |> List.foldBack
        (fun op terms ->
            let text =
                [
                    sprintf "The %s operator, %s." op.Name op.Description
                    "Example:"
                    sprintf "> %s" (fst op.Example)
                    snd op.Example
                ]
            let desc =
                match Map.tryFind op.Symbol terms with
                | Some existing -> existing @ text
                | None -> text
            Map.add op.Symbol desc terms)
        operators

let search term =
    match Map.tryFind term all with
    | Some info -> Ok info
    | None -> Error term
