module HexCalc.Terms

type Operation =
    | InfixOp of (bigint -> bigint -> bigint)
    | PrefixOp of (bigint -> bigint)

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
            "&", InfixOp (&&&), 1, "bitwise AND", "returns a number whose bits are set to one when the corresponding bit in both of the inputs are set to one", "0b111 & 0b110", "0b110"
            "|", InfixOp (|||), 1, "bitwise OR", "returns a number whose bits are set to one when when the corresponding bit in one of the two inputs is set to one", "0b1100 | 0b0110", "0b1110"
            "^", InfixOp (^^^), 2, "bitwise XOR", "returns a number whose bets are set to one when the corresponding bit in both of the inputs are different from each other", "0b1100 ^ 0b1011", "0b111"
            "+", InfixOp (+), 3, "addition", "adds two numbers together and returns the sum", "1 + 2", "3"
            "-", InfixOp (-), 3, "subtraction", "subtracts the right number from the left number and returns the difference", "0xA - 1", "9"
            "*", InfixOp (*), 4, "multiplication", "multiplies two numbers together and returns the product", "3 * 4", "12"
            "/", InfixOp (/), 4, "division", "divides the left number by the right number and returns the quotient", "9 / 0b11", "0b11"
            "%", InfixOp (%), 4, "modulo", "divides the left number by the right number and returns the remainder", "11 % 3", "2"
            "-", PrefixOp bigint.Negate, 5, "negation", "returns the opposite of its operand, which is the operand multiplied by -1", "-5", "-5"
        ]

type FunctionBody =
    | Arity1 of string * (Integer -> Integer)
    | Arity2 of string * string * (Integer -> Integer -> Integer)

    override this.ToString() =
        match this with
        | Arity1(arg, _) -> arg
        | Arity2(arg1, arg2, _) -> sprintf "%s, %s" arg1 arg2

type Function =
    { Body: FunctionBody
      Description: string
      Example: string * string
      Name: string }

    override this.ToString() =
        sprintf "%s(%O)" this.Name this.Body

let functions =
    List.map
        (fun (name, body, desc, example, result) ->
            { Body = body
              Description = desc
              Example = (example, result)
              Name = name })
        [
            let setbase nbase =
                Arity1("value", fun value -> { value with Base = nbase })
            "dec", setbase Base10, "Converts its argument into a decimal (base 10) integer.", "dec(0xA)", "10"
            "hex", setbase Base16, "Converts its argument into a hexadecimal (base 16) integer.", "hex(15)", "0xF"
            "bin", setbase Base2, "Converts its argument into a binary (base 2) integer.", "bin(3)", "0b11"
            "pow", Arity2("base", "exponent", fun ibase iexp -> { ibase with Value = bigint.Pow(ibase.Value, int iexp.Value) }), "Returns the first argument to the power of the second argument. Large exponents may cause the program to crash.", "pow(2, 4)", "16"
            "abs", Arity1("i", fun value -> { value with Value = abs value.Value }), "Returns the absolute value of an integer.", "abs(-7)", "7"
        ]

let all: Map<string, string list> =
    let defterm desc (example: string * string) =
        [
            yield! desc
            "Example:"
            sprintf "> %s" (fst example)
            snd example
        ]

    [
        for f in functions do
            let info =
                defterm
                    [ string f; f.Description ]
                    f.Example
            f.Name, info
    ]
    |> Map.ofList
    |> List.foldBack
        (fun (op: Operator) terms ->
            let text =
                defterm
                    [ sprintf "The %s operator, %s." op.Name op.Description ]
                    op.Example
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
