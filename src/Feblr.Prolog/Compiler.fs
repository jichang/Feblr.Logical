namespace Feblr.Prolog.Compiler

module Lexer =
    type Source =
        { code: char seq
          offset: int32 }

    type TokenValue =
        | Num of char seq
        | Atom of char seq
        | Variable of char seq
        | Str of char seq
        | LeftSquareBracket
        | RightSquareBracket
        | LeftBracket
        | RightBracket
        | Comma
        | Period
        | Whitespace
        | Newline

    type Marker =
        | NumMarker
        | AtomMarker
        | VariableMarker
        | StrMarker
        | LeftSquareBracketMarker
        | RightSquareBracketMarker
        | LeftBraketMarker
        | RightBracketMarker
        | CommaMarker
        | PeriodMarker
        | WhitespaceMarker
        | NewlineMarker
        | OtherMarker

    type Token =
        { value: TokenValue
          offset: int32 }

    let isDigit c =
        c >= '0' && c <= '9'
    
    let isLowerCase c =
        c >= 'a' && c <= 'z'

    let isUpperCase c =
        c >= 'A' && c <= 'Z'

    let isUnderscore c =
        c = '_'

    let isWhitespace c =
        c = ' '

    let isNewline c =
        c = '\n' || c = '\r'

    let isDot c =
        c = '.'

    let isQuote c =
        c = '`'

    let checkMark (c: char): Marker =
        if isDigit c then
            NumMarker
        else if isLowerCase c then
            AtomMarker
        else if isUpperCase c || isUnderscore c then
            VariableMarker
        else if isQuote c then
            StrMarker
        else if c = '[' then
            LeftSquareBracketMarker
        else if c = ']' then
            RightSquareBracketMarker
        else if c = '(' then
            LeftBraketMarker
        else if c = ')' then
            RightBracketMarker
        else if c = ',' then
            CommaMarker
        else if c = '.' then
            PeriodMarker
        else if c = ' ' then
            WhitespaceMarker
        else if isNewline c then
            NewlineMarker
        else
            OtherMarker

    type LexerError =
        { offset: int32
          expected: string
          actual: string }

    let parseNum (source: Source) =
        let integer =
            Seq.takeWhile isDigit source.code
        let length = Seq.length integer
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isDot nextChar then
                let decimal =
                    source.code
                    |> Seq.skip (length + 1)
                    |> Seq.takeWhile isDigit
                let num = Seq.concat [integer; Array.toSeq [|'.'|]; decimal]
                let length = Seq.length decimal + length + 1
                match Seq.tryItem (Seq.length decimal + length + 1) source.code with
                | Some nextChar ->
                    if isWhitespace nextChar || isNewline nextChar then
                        Ok ({ value = Num num; offset = source.offset }, length)
                    else
                        Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
                | None ->
                    Ok ({ value = Num num; offset = source.offset }, length)
            else if isWhitespace nextChar || isNewline nextChar then
                Ok ({ value = Num integer; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Num integer; offset = source.offset }, length)

    let parseAtom (source: Source) =
        let atom =
            Seq.takeWhile isLowerCase source.code
        let length = Seq.length atom
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar then
                Ok ({ value = Atom atom; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Atom atom; offset = source.offset }, length)

    let parseVariable (source: Source) =
        let variable =
            Seq.takeWhile (fun c -> isLowerCase c || isUpperCase c || isUnderscore c)source.code
        let length = Seq.length variable
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar then
                Ok ({ value = Variable variable; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Variable variable; offset = source.offset }, length)

    let parseString (source: Source) =
        let str =
            source.code
            |> Seq.skip 1
            |> Seq.takeWhile (isQuote >> not)
        let length = Seq.length str
        match Seq.tryItem (length + 2) source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar then
                Ok ({ value = Str str; offset = source.offset }, length + 2)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Str str; offset = source.offset }, length + 2)

    let parseOther (source: Source) =
        let atom =
            Seq.takeWhile (fun c -> not (isWhitespace c || isNewline c)) source.code
        Ok ({ value = Atom atom; offset = source.offset }, Seq.length atom)

    let rec lex (tokens: Token seq) (source: Source) =
        if Seq.isEmpty source.code then
            Ok tokens
        else
            let c = Seq.head source.code
            match checkMark c with
            | NumMarker ->
                match parseNum source with
                | Ok (token, len) ->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err
            | AtomMarker ->
                match parseAtom source with
                | Ok (token, len) ->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err
            | VariableMarker ->
                match parseVariable source with
                | Ok (token, len)->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err
            | StrMarker ->
                match parseString source with
                | Ok (token, len) ->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err
            | LeftSquareBracketMarker ->
                let token = { value = LeftSquareBracket; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | RightSquareBracketMarker ->
                let token = { value = RightSquareBracket; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | LeftBraketMarker ->
                let token = { value = LeftBracket; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | RightBracketMarker ->
                let token = { value = RightBracket; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | CommaMarker ->
                let token = { value = Comma; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | PeriodMarker ->
                let token = { value = Period; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | WhitespaceMarker ->
                let token = { value = Whitespace; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | NewlineMarker ->
                let token = { value = Newline; offset = source.offset }
                lex (Seq.append tokens [token]) ({ code = Seq.skip 1 source.code; offset = source.offset + 1})
            | OtherMarker ->
                match parseOther source with
                | Ok (token, len) ->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err

module Parser =
    let parse = ()

module Compiler =
    let compile = ()
