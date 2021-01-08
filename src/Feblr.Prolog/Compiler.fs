namespace Feblr.Prolog.Compiler

open System

module Syntax =
    let charsToString (chars: char seq) =
        chars |> Seq.toArray |> String

    type Source =
        { code: char seq
          offset: int32 }

    type Value =
        | Digits of string
        | Identifier of string
        | Variable of string
        | Chars of string
        | Operators of string
        | LeftSquareBracket
        | RightSquareBracket
        | LeftBracket
        | RightBracket
        | Comma
        | Period
        | Whitespace
        | Newline

    type Marker =
        | DigitsMarker
        | IdentifierMarker
        | VariableMarker
        | CharsMarker
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
        { value: Value
          offset: int32 }

    let isDigit c =
        c >= '0' && c <= '9'

    let isAlphabet c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    
    let isLowerCase c =
        c >= 'a' && c <= 'z'

    let isUpperCase c =
        c >= 'A' && c <= 'Z'

    let isUnderscore c =
        c = '_'

    let isLeftBracket c =
        c = '('

    let isRightBracket c =
        c = ')'

    let isLeftSquareBracket c =
        c = '['

    let isRightSquareBracket c =
        c = ']'

    let isWhitespace c =
        c = ' '

    let isNewline c =
        c = '\n' || c = '\r'

    let isQuote c =
        c = '`'

    let isPeriod c =
        c = '.'

    let isComma c =
        c = ','

    let checkMark (c: char): Marker =
        if isDigit c then
            DigitsMarker
        else if isLowerCase c then
            IdentifierMarker
        else if isUpperCase c || isUnderscore c then
            VariableMarker
        else if isQuote c then
            CharsMarker
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

    type SyntaxError =
        { offset: int32
          expected: string
          actual: string }

    let parseDigits (source: Source) =
        let integer =
            Seq.takeWhile isDigit source.code
        let length = Seq.length integer
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isPeriod nextChar then
                let decimal =
                    source.code
                    |> Seq.skip (length + 1)
                    |> Seq.takeWhile isDigit
                let digits = Seq.concat [integer; Array.toSeq [|'.'|]; decimal]
                let length = Seq.length decimal + length + 1
                match Seq.tryItem (Seq.length decimal + length + 1) source.code with
                | Some nextChar ->
                    if isWhitespace nextChar || isNewline nextChar then
                        Ok ({ value = digits |> charsToString |> Digits; offset = source.offset }, length)
                    else
                        Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
                | None ->
                    Ok ({ value = digits |> charsToString |> Digits; offset = source.offset }, length)
            else if isWhitespace nextChar || isNewline nextChar then
                Ok ({ value = integer |> charsToString |> Digits; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = integer |> charsToString |> Digits; offset = source.offset }, length)

    let parseIdentifier (source: Source) =
        let identifier =
            Seq.takeWhile (fun c ->isAlphabet c || isDigit c) source.code
        let length = Seq.length identifier
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar || isPeriod nextChar || isLeftBracket nextChar || isRightBracket nextChar || isComma nextChar || isRightSquareBracket nextChar then
                Ok ({ value = identifier |> charsToString |> Identifier; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = identifier |> charsToString |> Identifier; offset = source.offset }, length)

    let parseVariable (source: Source) =
        let variable =
            Seq.takeWhile (fun c -> isAlphabet c || isUnderscore c || isDigit c)source.code
        let length = Seq.length variable
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar || isPeriod nextChar then
                Ok ({ value = variable |> charsToString |> Variable; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = variable |> charsToString |> Variable; offset = source.offset }, length)

    let parseString (source: Source) =
        let chars =
            source.code
            |> Seq.skip 1
            |> Seq.takeWhile (isQuote >> not)
        let length = Seq.length chars
        match Seq.tryItem (length + 2) source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar|| isPeriod nextChar || isRightBracket nextChar || isComma nextChar || isRightSquareBracket nextChar then
                Ok ({ value = chars |> charsToString |> Chars; offset = source.offset }, length + 2)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = chars |> charsToString |> Chars; offset = source.offset }, length + 2)

    let parseOther (source: Source) =
        let identifier =
            Seq.takeWhile (fun c -> not (isWhitespace c || isNewline c)) source.code
        Ok ({ value = identifier |> charsToString |> Identifier; offset = source.offset }, Seq.length identifier)

    let rec lex (tokens: Token seq) (source: Source) =
        if Seq.isEmpty source.code then
            Ok tokens
        else
            let c = Seq.head source.code
            match checkMark c with
            | DigitsMarker ->
                match parseDigits source with
                | Ok (token, len) ->
                    lex (Seq.append tokens [token]) ({ code = Seq.skip len source.code; offset = source.offset + len})
                | Error err ->
                    Error err
            | IdentifierMarker ->
                match parseIdentifier source with
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
            | CharsMarker ->
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

module rec Grammar =
    open Syntax

    type Term =
        | Var of string (* Variable *)
        | Str of string (* `hello, world` *)
        | Num of string  (* 103323434 *)
        | List of Term seq (* [...] *)
        | Atom of string * Term seq (* functor(t1, t2). *)
        | CompoundTerm of string * Term seq * Term seq (* functor(t1, t2) := functor2, X, `hello`, 203430 *)

    type Fact =
        { functor: string
          arguments: Term seq }

    type Rule =
        { functor: string
          arguments: Term seq
          predicates: Term seq }

    type Clause =
        | Fact of Fact
        | Rule of Rule

    type GrammarError =
        { token: Token option
          message: string }

    let isInsignificantToken token =
        match token.value with
        | Newline
        | Whitespace -> true
        | _ -> false

    let trim (tokens: Token seq) =
        tokens
        |> Seq.skipWhile isInsignificantToken

    let rec parseTuple (beginValue: Value) (endValue: Value) (terms: Term seq) (tokens: Token seq) =
        let tokens = trim tokens
        match Seq.tryHead tokens with
        | Some token ->
            if token.value = beginValue then
                match parseTerm (tokens |> Seq.tail |> trim) with
                | Ok (term, tokens) ->
                    let newTerms = Seq.append terms [term]
                    let tail = trim tokens
                    match Seq.tryHead tail with
                    | Some nextToken ->
                        if nextToken.value = Comma then
                            parseTuple beginValue endValue newTerms (tail |> Seq.tail |> trim)
                        else if nextToken.value = endValue then
                            Ok (newTerms, tail |> Seq.tail |> trim)
                        else
                            Error { token = Some token; message = $"tuple should end with {endValue}" }
                    | None ->
                        Ok (newTerms, tail)
                | Error err ->
                    Error err
            else
                Error { token = Some token; message = $"tuple should start with {beginValue}" }
        | None ->
            Error { token = None; message = $"tuple should start with {beginValue}" }


    let parseTerm (tokens: Token seq) =
        match Seq.tryHead tokens with
        | Some firstToken ->
            match firstToken.value with
            | Variable variable ->
                Ok (Var variable, Seq.tail tokens)
            | Digits digits ->
                Ok (Num digits, Seq.tail tokens)
            | Chars chars ->
                Ok (Str chars, Seq.tail tokens)
            | RightSquareBracket ->
                match parseTuple RightSquareBracket LeftSquareBracket [] tokens with
                | Ok (terms, tokens) ->
                    Ok (List terms, tokens)
                | Error error ->
                    Error error
            | Identifier functor ->
                let tail = Seq.tail tokens |> trim
                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | LeftBracket ->
                        match parseTuple LeftBracket RightBracket [] tail with
                        | Ok (arguments, tokens) ->
                            let tail = tokens |> Seq.tail |> trim
                            match Seq.tryHead tail with
                            | Some token ->
                                if token.value = Period then
                                    let compoundTerm = CompoundTerm (functor, arguments, [])
                                    Ok (compoundTerm, Seq.tail tail)
                                else if token.value = (Chars ":=") then
                                    match parseTuple (Chars ":=") Period [] tail with
                                    | Ok (predicates, tokens) ->
                                        let compoundTerm = CompoundTerm (functor, arguments, predicates)
                                        Ok (compoundTerm, tokens |> trim)
                                    | Error err ->
                                        Error err
                                else
                                    Error { token = Some token; message = "term should end with period or follow by := predicates"}
                            | None ->
                                Error { token = None; message = "term should end with period or follow by := predicates"}
                        | Error err ->
                            Error err
                    | Period ->
                        let atom = Atom (functor, [])
                        Ok (atom, tail |> Seq.tail)
                    | _ ->
                        Error { token = Some nextToken; message = "term should be separated by , or end with ." }
                | None ->
                    Error { token = None; message = "term should be separated by , or end with ." }
            | _ ->
                Error { token = Some firstToken; message = "term should start with identifier" }
        | None ->
            Error { token = None; message = "term should start with identifier" }

    let rec parse (clauses: Clause seq) (tokens: Token seq) =
        if Seq.isEmpty tokens then
            Ok clauses
        else
            let token = Seq.head tokens
            match token.value with
            | Identifier _ ->
                match parseTerm tokens with
                | Ok (term, tokens) ->
                    match term with
                    | Atom (functor, arguments) ->
                        let fact = { functor = functor; arguments = arguments }
                        parse (Seq.append clauses [Fact fact]) tokens
                    | CompoundTerm (functor, arguments, predicates) ->
                        let rule = { functor = functor; arguments = arguments; predicates = predicates }
                        parse (Seq.append clauses [Rule rule]) tokens
                    | _ ->
                        Error { token = Some token; message = "program should contains valid fact or rule" }
                | Error err ->
                    Error err
            | Whitespace
            | Newline ->
                parse clauses (Seq.tail tokens)
            | _ ->
                Error { token = Some token; message = "program should contains valid fact or rule" }

module Compiler =
    open Grammar

