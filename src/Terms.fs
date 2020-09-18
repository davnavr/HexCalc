module HexCalc.Terms

type Term<'Info> =
    { Description: string
      Example: string * string
      Info: 'Info
      Name: string }

type InfixOp =
    { Operation: Integer -> Integer -> Integer
      Precedence: int
      Symbol: string }

type PrefixOp =
    { Operation: Integer -> Integer
      Symbol: string }

type Operator =
    | InfixOp of InfixOp
    | PrefixOp of PrefixOp

    member this.Symbol =
        match this with
        | InfixOp { Symbol = symbol }
        | PrefixOp { Symbol = symbol } -> symbol

let operators =
    List.map
        (fun (name, desc, example, result, op: Operator) ->
            { Description = desc
              Example = (example, result)
              Info = op
              Name = name })
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

let all =
    List.foldBack
        (fun (op: Term<Operator>) terms ->
            let text =
                [
                    sprintf "The %s operator, %s." op.Name op.Description
                    "Example:"
                    sprintf "> %s" (fst op.Example)
                    snd op.Example
                ]
            let desc =
                match Map.tryFind op.Info.Symbol terms with
                | Some existing -> existing @ text
                | None -> text
            Map.add op.Info.Symbol desc terms)
        operators
        Map.empty

let search term =
    match Map.tryFind term all with
    | Some info -> Ok info
    | None -> Error term
