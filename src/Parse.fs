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
    
    let atEnd r = pos r >= r.String.Length

    let nextStr len r =
        let str =
            if pos r + len <= r.String.Length then
                r.String.Substring(pos r, len) |> Ok
            else
                string r |> Error
        move len r, str
    let nextChr r =
        if atEnd r
        then None
        else Some(move 1 r, r.String.Chars(pos r))

type ParserErrorMessage =
    | UnexpectedEnd
    | UnexpectedChar of char
    | UnexpectedString of exp: string * act: string
    | ErrorMessage of string

    override this.ToString() =
        match this with
        | UnexpectedEnd -> "Unexpected end of input"
        | UnexpectedChar c ->
            sprintf "Unexpected '%c' in input" c
        | UnexpectedString(exp, "") ->
            sprintf "Expected '%s', but the end of input" exp
        | UnexpectedString(exp, act) ->
            sprintf "Expected '%s', but got '%s'" exp act
        | ErrorMessage msg -> msg

type ParserError =
    { Context: string option
      Message: ParserErrorMessage }

    override this.ToString() =
        match this with
        | { Context = None; Message = err } ->
            string err
        | { Context = Some ctx; Message = ErrorMessage msg } ->
            sprintf "Error while parsing %s: %s" ctx msg
        | { Context = Some ctx; Message = err } ->
            sprintf "%O while parsing %s" err ctx

module ParserError =
    let ofmsg err = { Context = None; Message = err }

type Parser<'Result> = Reader -> Reader * Result<'Result, ParserError>

let runstr str (p: Parser<_>) =
    let reader, result =
        p { Position = 0u; String = str }
    match result with
    | Ok item -> Ok item
    | Error err -> Error(reader, err)

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
let inline (.>>) p1 p2 =
    p1 .>>. p2 |>> fst

let (<?>) (p: Parser<_>) name: Parser<_> =
    fun reader ->
        let reader', result = p reader
        let result' =
            Result.mapError
                (fun err -> { err with Context = Some name })
                result
        reader', result'

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
let choiceL ps name = choice ps <?> name

let attempt (p: Parser<_>): Parser<_> =
    fun reader ->
        match p reader with
        | (reader', Ok item) -> reader', Ok item
        | (_, err) -> reader, err

let forwarded() =
    let rp: Parser<_> ref =
        ref (fun _ -> invalidOp "The forwarded parser was not initalized")
    let r: Parser<_> =
        fun reader -> reader |> !rp
    r, rp

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
    //<?> "HEADMANY1" // NOTE: This p fails when parsing expression.
    >>= fun head ->
        many p |>> fun tail -> head :: tail

let sepBy1 p sep =
    p
    //<?> "HEADSEPBY1" // NOTE: This fails too, and might hide HEADMANY1?
    >>= fun head ->
        many (sep >>. p) |>> fun tail -> head :: tail

let anych: Parser<char> =
    fun reader ->
        match Reader.nextChr reader with
        | None ->
            reader, ParserError.ofmsg UnexpectedEnd |> Error
        | Some(reader', ch) ->
            reader', Ok ch
let chr c: Parser<char> =
    anych
    >>= fun act ->
        if act = c then
            retn c
        else
            UnexpectedChar act
            |> ParserError.ofmsg
            |> fail
let chrci c: Parser<char> =
    anych
    >>= fun act ->
        if Char.ToLowerInvariant act = Char.ToLowerInvariant c
        then retn act
        else
            UnexpectedChar act
            |> ParserError.ofmsg
            |> fail

let spaces: Parser<unit> =
    fun reader ->
        let amt =
            [| ' ' |]
            |> (string reader).TrimStart
            |> String.length
        Reader.move amt reader, Ok()

let private strwhen cond (s: string) reader =
    match Reader.nextStr s.Length reader with
    | (reader', Ok next) when cond s next -> reader', Ok next
    | (reader', Ok bad)
    | (reader', Error bad) ->
        let err =
            UnexpectedString(s, bad)
            |> ParserError.ofmsg
            |> Error
        reader', err
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
            | (digit: int :: tail1) :: tail2 ->
                inner (append digit) (tail1 :: tail2)
        inner bigint.Zero
    let digits ibase (ds: char list) =
        let digit =
            List.mapi
                (fun i d ->
                    chrci d
                    |> attempt
                    >>% i)
                ds
            |> choice
            <?> "digit"
        sepBy1
            (many1 digit) // TODO: For some reason, this parser keeps consuming until end of input when evaluating expression.
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
    .>>. choiceL
        [
            let suffix symbol itype vtype: Parser<bigint -> _> =
                strci symbol
                |> attempt
                >>% (itype >> vtype)
            
            suffix "uy" uint8 Value.UInt8
            suffix "ul" uint64 Value.UInt64
            suffix "u" uint32 Value.UInt32
            suffix "i" id Value.Big
            suffix "l" int64 Value.Int64
            retn (int32 >> Value.Int32)
        ]
        "suffix"
    >>= fun ((ibase, value), f) ->
        try
            let result =
                { Base = ibase
                  Value = f value }
            retn result
        with
            | :? OverflowException ->
                sprintf
                    "'%A' is outside the range of allowed values for this type of integer."
                    value
                |> ErrorMessage
                |> ParserError.ofmsg
                |> fail
    <?> "integer"
