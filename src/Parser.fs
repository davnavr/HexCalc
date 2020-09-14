module HexCalc.Parser

type Reader(str: string) = // TODO: Should this be immutable?
    let mutable pos = 0

    member _.Position
        with get() = pos
        and set value = pos <- value

    override _.ToString() = str.Substring pos

    member this.NextString len =
        if pos + len <= str.Length then
            str.Substring(pos, len) |> Ok
        else
            string this |> Error

type ParseResult<'Result> =
    | Success of 'Result
    | UnexpectedChar of char
    | UnexpectedString of exp: string * act: string

let (|ParseSuccess|ParseError|) =
    function
    | Success result -> Choice1Of2 result
    | err -> Choice2Of2 err

type Parser<'Result> = Reader -> ParseResult<'Result>

let str (s: string) (reader: Reader) =
    match reader.NextString s.Length with
    | Ok next when next = s -> Success s
    | Ok bad
    | Error bad -> UnexpectedString(s, bad)

let choice ps (reader: Reader) =
    match ps with
    | [] -> invalidArg "ps" "Parser list must not be empty"
    | [ p ] -> p reader
    | p :: tail ->
        let origin = reader.Position
        let rec inner p tail =
            match (p reader, tail) with
            | (ParseSuccess result, _) ->
                Success result
            | (ParseError err, []) -> err
            | (ParseError _, np :: ntail) ->
                reader.Position <- origin
                inner np ntail
        inner p tail
