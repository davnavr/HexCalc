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
        let str =
            if pos r + len <= r.String.Length then
                r.String.Substring(pos r, len) |> Ok
            else
                string r |> Error
        move len r, str

    let atEnd r = pos r >= r.String.Length

type ParserError =
    | UnexpectedEnd
    | UnexpectedChar of char
    | UnexpectedString of exp: string * act: string
    | ErrorMessage of string

    override this.ToString() =
        match this with
        | UnexpectedEnd -> "Unexpected end of input"
        | UnexpectedChar c ->
            sprintf "Unexpected '%c' in input" c
        | UnexpectedString(exp, act) ->
            sprintf "Expected '%s', but got '%s'" exp act
        | ErrorMessage msg -> msg

type Parser<'Result> = Reader -> Reader * Result<'Result, ParserError>

let runstr str (p: Parser<_>) =
    p { Position = 0u; String = str } |> snd

let retn value: Parser<_> = fun reader -> reader, Ok value
let fail err: Parser<_> = fun reader -> reader, Error err

let (>>=) (p: Parser<_>) (binder: _ -> Parser<_>): Parser<_> =
    fun reader ->
        let reader', result = p reader
        match result with
        | Ok item -> binder item reader'
        | Error err -> reader', Error err

let inline (>>%) p item = p >>= fun _ -> retn item
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
            let reader', result = p reader
            match (result, tail) with
            | (Ok item, _) -> reader', Ok item
            | (err, _) when reader'.Position <> reader.Position ->
                reader', err
            | (err, []) -> reader', err
            | (_, p' :: tail') ->
                inner p' tail'
        inner p tail

let attempt (p: Parser<_>): Parser<_> =
    fun reader ->
        match p reader with
        | (reader', Ok item) -> reader', Ok item
        | (_, err) -> reader, err

let many (p: Parser<_>): Parser<_ list> =
    let rec inner results reader =
        match p reader with
        | (reader', Ok item) ->
            inner (item :: results) reader'
        | (_, Error _) ->
            reader, List.rev results |> Ok
    inner []
let many1 p: Parser<_ list> =
    p
    >>= fun head ->
        many p |>> fun tail -> head :: tail

let sepBy1 p sep =
    p
    >>= fun head ->
        many (sep >>. p) |>> fun tail -> head :: tail

let chr c: Parser<char> =
    fun reader ->
        if Reader.atEnd reader then
            reader, Error UnexpectedEnd
        else
            let result = reader.String.Chars (Reader.pos reader)
            let reader' = Reader.move 1 reader
            if result = c then
                reader', Ok c
            else
                reader', UnexpectedChar result |> Error

let private strwhen cond (s: string) reader =
    match Reader.nextStr s.Length reader with
    | (reader', Ok next) when cond s next -> reader', Ok next
    | (reader', Ok bad)
    | (reader', Error bad) -> reader', UnexpectedString(s, bad) |> Error
let str: _ -> Parser<string> =
    strwhen (=)
let strci: _ -> Parser<string> =
    strwhen (fun exp act -> exp.Equals(act, StringComparison.OrdinalIgnoreCase))

let integer: Parser<Integer> =
    let buildint (nbase: int) =
        let rec inner num =
            let append (digit: int) =
                (num * bigint nbase) + bigint digit
            function
            | [] -> num
            | [] :: tail -> inner num tail
            | (digit :: tail) :: [] ->
                inner (append digit) [ tail ]
            | (digit: int :: tail1) :: tail2 ->
                inner (append digit) (tail1 :: tail2.Tail)
        inner bigint.Zero
    let digits ibase (ds: char list) =
        let digit =
            List.mapi
                (fun i d ->
                    string d
                    |> strci
                    |> attempt
                    >>% i)
                ds
            |> choice
        sepBy1
            (many1 digit)
            (chr '_' |> many)
        |>> (fun digits ->
            ibase, buildint (List.length ds) digits)
    choice
        [
            strci "0b"
            |> attempt
            >>. digits Base2 [ '0'; '1' ]

            strci "0x"
            |> attempt
            >>. digits Base16 ([ '0'..'9' ] @ [ 'a'..'f' ])

            digits Base10 [ '0'..'9' ]
        ]
    .>>. choice
        [
            retn (int32<bigint> >> Value.Int32)
        ]
    >>= fun ((ibase, value), f) ->
        try
            let result =
                { Base = ibase
                  Value = f value }
            retn result
        with
            | :? OverflowException ->
                sprintf
                    "'%A' is outside the range of allowed valuse for this type of integer."
                    value
                |> ErrorMessage
                |> fail
