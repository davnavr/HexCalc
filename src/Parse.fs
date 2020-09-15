module HexCalc.Parse

open System

type Reader =
    { Position: uint
      String: string }

    override this.ToString() = this.String.Substring(int this.Position)

[<RequireQualifiedAccess>]
module Reader =
    let inline pos r = int r.Position

    let inline move amt r =
        { r with Position = r.Position + uint amt }

    let nextStr len r =
        if pos r + len <= r.String.Length then
            Ok(r.String.Substring(pos r, len), move len r)
        else
            string r |> Error

    let atEnd r = pos r >= r.String.Length

type ParserError =
    | UnexpectedEnd
    | UnexpectedChar of char
    | UnexpectedString of exp: string * act: string

    override this.ToString() =
        match this with
        | UnexpectedEnd -> "Unexpected end of input"
        | UnexpectedChar c ->
            sprintf "Unexpected '%c' in input" c
        | UnexpectedString(exp, act) ->
            sprintf "Expected '%s', but got '%s'" exp act

type Parser<'Result> = Reader -> Result<'Result * Reader, ParserError>

let private strwhen cond (s: string) reader =
    match Reader.nextStr s.Length reader with
    | Ok (next, r) when cond s next -> Ok(s, r)
    | Ok (bad, _)
    | Error bad -> UnexpectedString(s, bad) |> Error

let chr c: Parser<char> =
    fun reader ->
        if Reader.atEnd reader then
            Error UnexpectedEnd
        else
            let result = reader.String.Chars (Reader.pos reader)
            if result = c then
                Ok(c, Reader.move 1 reader)
            else
                UnexpectedChar result |> Error

let str: _ -> Parser<string> =
    strwhen (=)
let strci: _ -> Parser<string> =
    strwhen (fun exp act -> exp.Equals(act, StringComparison.OrdinalIgnoreCase))

let retn value: Parser<_> = fun reader -> Ok(value, reader)

let (>>=) (p: Parser<_>) (binder: _ -> Parser<_>): Parser<_> =
    fun reader ->
        Result.bind
            (fun (result, reader') -> binder result reader')
            (p reader)

let inline (|>>) p map =
    p >>= (map >> retn)
let inline (.>>.) p1 p2: Parser<_ * _> =
    p1 >>= (fun r1 -> p2 |>> (fun r2 -> r1, r2))
let inline (>>.) p1 p2 =
    p1 .>>. p2 |>> snd

let apply2 p pf: Parser<_> =
    p
    >>= fun result ->
        pf |>> fun f -> f result

let choice (ps: Parser<_> list) reader =
    match ps with
    | [] -> invalidArg "ps" "The parser list must not be empty"
    | [ p ] -> p reader
    | p :: tail ->
        let rec inner p tail =
            match (p reader, tail) with
            | (Ok result, _) ->
                Ok result
            | (err, []) -> err
            | (_, p' :: tail') ->
                inner p' tail'
        inner p tail

let many p: Parser<_ list> =
    let rec inner results reader =
        match p reader with
        | Ok (item, reader') ->
            inner (item :: results) reader'
        | Error _ ->
            Ok(List.rev results, reader)
    inner []
let many1 p: Parser<_ list> =
    p
    >>= fun head ->
        many p |>> fun tail -> head :: tail

let sepBy1 p sep =
    p
    >>= fun head ->
        many (sep >>. p) |>> fun tail -> head :: tail

let dec =
    List.map chr [ '0'..'9' ] |> choice

let integer =
    let digits pd =
        sepBy1
            (many1 pd)
            (chr '_' |> many)
        |>> List.collect id
    choice
        [
            //strci "0b"

            //strci "0x"

            digits dec
        ]
