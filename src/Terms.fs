[<RequireQualifiedAccess>]
module HexCalc.Terms

let all =
    [|
        "+",
        [
            "The addition operator, adds two numbers together and returns the sum."
            "Example:"
            "> 1 + 2"
            "3"
        ]
        "-",
        [
            "The subtraction operator, subtracts the right number from the left number and returns the difference."
            "Example:"
            "> 0xA - 1"
            "9"
        ]
        "*",
        [
            "The multiplication operator, multiplies two numbers together and returns the product."
            "Has higher precedence than addition or subtraction."
            "Example:"
            "> 3 * 4"
            "12"
        ]
        "/",
        [
            "The division operator, divides the left number by the right number and returns the quotient."
            "Has higher precedence than addition or subtraction."
            "Example:"
            "> 9 / 0b11"
            "3"
        ]
    |]
    |> Map.ofArray

let search =
    function
    | Some "all" ->
        Map.toList all
        |> List.map fst
        |> Ok
        |> Some
    | Some term ->
        match Map.tryFind term all with
        | Some desc -> Ok desc |> Some
        | None -> Error term |> Some
    | None -> None
