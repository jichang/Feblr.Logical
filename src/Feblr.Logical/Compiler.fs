namespace Feblr.Logical.Compiler

open System

module Syntax =
    let charsToString (chars: char seq) = chars |> Seq.toArray |> String

    type Source = { code: char seq; offset: int32 }

    type Value =
        | Digits of string
        | Identifier of string
        | Variable of string
        | Chars of string
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
        | OperatorMarker

    type Token = { value: Value; offset: int32 }

    let isDigit c = c >= '0' && c <= '9'

    let isAlphabet c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    let isLowerCase c = c >= 'a' && c <= 'z'

    let isUpperCase c = c >= 'A' && c <= 'Z'

    let isUnderscore c = c = '_'

    let isLeftBracket c = c = '('

    let isRightBracket c = c = ')'

    let isLeftSquareBracket c = c = '['

    let isRightSquareBracket c = c = ']'

    let isWhitespace c = c = ' '

    let isNewline c = c = '\n' || c = '\r'

    let isQuote c = c = '`'

    let isPeriod c = c = '.'

    let isComma c = c = ','

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
            OperatorMarker

    type SyntaxError =
        { offset: int32
          expected: string
          actual: string }

    let parseDigits (source: Source) =
        let integer = Seq.takeWhile isDigit source.code
        let length = Seq.length integer

        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isPeriod nextChar then
                let decimal =
                    source.code
                    |> Seq.skip (length + 1)
                    |> Seq.takeWhile isDigit

                let digits =
                    Seq.concat [ integer
                                 Array.toSeq [| '.' |]
                                 decimal ]

                let length = Seq.length decimal + length + 1

                match Seq.tryItem (Seq.length decimal + length + 1) source.code with
                | Some nextChar ->
                    if isWhitespace nextChar || isNewline nextChar then
                        Ok(
                            { value = digits |> charsToString |> Digits
                              offset = source.offset },
                            length
                        )
                    else
                        Error
                            { offset = source.offset + length
                              expected = "whitespace or newline"
                              actual = $"{nextChar}" }
                | None ->
                    Ok(
                        { value = digits |> charsToString |> Digits
                          offset = source.offset },
                        length
                    )
            else if isWhitespace nextChar || isNewline nextChar then
                Ok(
                    { value = integer |> charsToString |> Digits
                      offset = source.offset },
                    length
                )
            else
                Error
                    { offset = source.offset + length
                      expected = "whitespace or newline"
                      actual = $"{nextChar}" }
        | None ->
            Ok(
                { value = integer |> charsToString |> Digits
                  offset = source.offset },
                length
            )

    let parseIdentifier (source: Source) =
        let identifier =
            Seq.takeWhile (fun c -> isAlphabet c || isDigit c) source.code

        let length = Seq.length identifier

        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar
               || isNewline nextChar
               || isPeriod nextChar
               || isLeftBracket nextChar
               || isRightBracket nextChar
               || isComma nextChar
               || isRightSquareBracket nextChar then
                Ok(
                    { value = identifier |> charsToString |> Identifier
                      offset = source.offset },
                    length
                )
            else
                Error
                    { offset = source.offset + length
                      expected = "whitespace or newline"
                      actual = $"{nextChar}" }
        | None ->
            Ok(
                { value = identifier |> charsToString |> Identifier
                  offset = source.offset },
                length
            )

    let parseVariable (source: Source) =
        let variable =
            Seq.takeWhile (fun c -> isAlphabet c || isUnderscore c || isDigit c) source.code

        let length = Seq.length variable

        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar
               || isNewline nextChar
               || isPeriod nextChar then
                Ok(
                    { value = variable |> charsToString |> Variable
                      offset = source.offset },
                    length
                )
            else
                Error
                    { offset = source.offset + length
                      expected = "whitespace or newline"
                      actual = $"{nextChar}" }
        | None ->
            Ok(
                { value = variable |> charsToString |> Variable
                  offset = source.offset },
                length
            )

    let parseString (source: Source) =
        let chars =
            source.code
            |> Seq.skip 1
            |> Seq.takeWhile (isQuote >> not)

        let length = Seq.length chars

        match Seq.tryItem (length + 2) source.code with
        | Some nextChar ->
            if isWhitespace nextChar
               || isNewline nextChar
               || isPeriod nextChar
               || isRightBracket nextChar
               || isComma nextChar
               || isRightSquareBracket nextChar then
                Ok(
                    { value = chars |> charsToString |> Chars
                      offset = source.offset },
                    length + 2
                )
            else
                Error
                    { offset = source.offset + length
                      expected = "whitespace or newline"
                      actual = $"{nextChar}" }
        | None ->
            Ok(
                { value = chars |> charsToString |> Chars
                  offset = source.offset },
                length + 2
            )

    let parseOperator (source: Source) =
        let operator =
            Seq.takeWhile (fun c -> not (isWhitespace c || isNewline c)) source.code

        Ok(
            { value = operator |> charsToString |> Identifier
              offset = source.offset },
            Seq.length operator
        )

    let rec lex (tokens: Token seq) (source: Source) =
        if Seq.isEmpty source.code then
            Ok tokens
        else
            let c = Seq.head source.code

            match checkMark c with
            | DigitsMarker ->
                match parseDigits source with
                | Ok (token, len) ->
                    lex
                        (Seq.append tokens [ token ])
                        ({ code = Seq.skip len source.code
                           offset = source.offset + len })
                | Error err -> Error err
            | IdentifierMarker ->
                match parseIdentifier source with
                | Ok (token, len) ->
                    lex
                        (Seq.append tokens [ token ])
                        ({ code = Seq.skip len source.code
                           offset = source.offset + len })
                | Error err -> Error err
            | VariableMarker ->
                match parseVariable source with
                | Ok (token, len) ->
                    lex
                        (Seq.append tokens [ token ])
                        ({ code = Seq.skip len source.code
                           offset = source.offset + len })
                | Error err -> Error err
            | CharsMarker ->
                match parseString source with
                | Ok (token, len) ->
                    lex
                        (Seq.append tokens [ token ])
                        ({ code = Seq.skip len source.code
                           offset = source.offset + len })
                | Error err -> Error err
            | LeftSquareBracketMarker ->
                let token =
                    { value = LeftSquareBracket
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | RightSquareBracketMarker ->
                let token =
                    { value = RightSquareBracket
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | LeftBraketMarker ->
                let token =
                    { value = LeftBracket
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | RightBracketMarker ->
                let token =
                    { value = RightBracket
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | CommaMarker ->
                let token =
                    { value = Comma
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | PeriodMarker ->
                let token =
                    { value = Period
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | WhitespaceMarker ->
                let token =
                    { value = Whitespace
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | NewlineMarker ->
                let token =
                    { value = Newline
                      offset = source.offset }

                lex
                    (Seq.append tokens [ token ])
                    ({ code = Seq.skip 1 source.code
                       offset = source.offset + 1 })
            | OperatorMarker ->
                match parseOperator source with
                | Ok (token, len) ->
                    lex
                        (Seq.append tokens [ token ])
                        ({ code = Seq.skip len source.code
                           offset = source.offset + len })
                | Error err -> Error err

module rec Grammar =
    open Syntax

    let ifTerm = ":-"
    type Term =
        | Var of string (* Variable *)
        | Str of string (* `hello, world` *)
        | Num of string (* 103323434 *)
        | Atom of string (* atom *)
        | List of Term seq (* [...] *)
        | CompoundTerm of string * Term seq * Term seq (* functor(t1, t2) := functor2, X, `hello`, 203430 *)

    type Clause =
        { functor: string
          arguments: Term seq
          predicates: Term seq }

    type GrammarError =
        { token: Token option
          message: string }

    let isInsignificantToken token =
        match token.value with
        | Newline
        | Whitespace -> true
        | _ -> false

    let trim (tokens: Token seq) =
        tokens |> Seq.skipWhile isInsignificantToken

    let rec parseTuple (terms: Term seq) (tokens: Token seq) =
        match parseTerm tokens with
        | Ok (term, newTokens) ->
            let newTerms = Seq.append terms [ term ]
            let newTail = trim newTokens

            match Seq.tryHead newTail with
            | Some nextToken ->
                if nextToken.value = Comma then
                    parseTuple newTerms (newTail |> Seq.tail |> trim)
                else
                    Ok(newTerms, newTail)
            | None -> Ok(newTerms, newTail)
        | Error err -> Error err


    let parseTerm (tokens: Token seq) =
        match Seq.tryHead tokens with
        | Some firstToken ->
            match firstToken.value with
            | Variable variable -> Ok(Var variable, Seq.tail tokens)
            | Digits digits -> Ok(Num digits, Seq.tail tokens)
            | Chars chars -> Ok(Str chars, Seq.tail tokens)
            | LeftSquareBracket ->
                let tail = Seq.tail tokens |> trim

                match Seq.tryHead tail with
                | Some nextToken ->
                    if nextToken.value = RightSquareBracket then
                        Ok(List [], tail |> Seq.tail)
                    else
                        match parseTuple [] tail with
                        | Ok (terms, tokens) ->
                            match Seq.tryHead tokens with
                            | Some nextToken ->
                                if nextToken.value = RightSquareBracket then
                                    Ok(List terms, tokens |> Seq.tail |> trim)
                                else
                                    Error
                                        { token = Some nextToken
                                          message = "list should end with ]" }
                            | None -> Ok(List terms, tokens)
                        | Error error -> Error error
                | None ->
                    Error
                        { token = None
                          message = "list should end with ]" }
            | Identifier functor ->
                let tail = Seq.tail tokens |> trim

                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | Identifier _
                    | Variable _
                    | Digits _
                    | Chars _ ->
                        Error
                            { token = Some nextToken
                              message = "term should be separated by ," }
                    | LeftBracket ->
                        let newTail = Seq.tail tail |> trim

                        match Seq.tryHead newTail with
                        | Some nextToken ->
                            if nextToken.value = RightBracket then
                                let tail = newTail |> Seq.tail |> trim

                                match Seq.tryHead tail with
                                | Some nextToken ->
                                    if nextToken.value = Period then
                                        let compoundTerm = CompoundTerm(functor, [], [])
                                        Ok(compoundTerm, tail)
                                    else if nextToken.value = Identifier ifTerm then
                                        let newTail = tail |> Seq.tail |> trim

                                        match parseTuple [] newTail with
                                        | Ok (predicates, tail) ->
                                            let term = CompoundTerm(functor, [], predicates)
                                            Ok(term, tail |> trim)
                                        | Error err -> Error err
                                    else
                                        Error
                                            { token = Some nextToken
                                              message = "compound term should end with ." }
                                | None ->
                                    let compoundTerm = CompoundTerm(functor, [], [])
                                    Ok(compoundTerm, tail)
                            else
                                match parseTuple [] newTail with
                                | Ok (arguments, tail) ->
                                    match Seq.tryHead tail with
                                    | Some nextToken ->
                                        if nextToken.value = RightBracket then
                                            let compoundTerm = CompoundTerm(functor, arguments, [])
                                            let newTail = tail |> Seq.tail |> trim

                                            match Seq.tryHead newTail with
                                            | Some nextToken ->
                                                if nextToken.value = Period then
                                                    Ok(compoundTerm, newTail)
                                                else if nextToken.value = Identifier ifTerm then
                                                    let newTail = newTail |> Seq.tail |> trim

                                                    match parseTuple [] newTail with
                                                    | Ok (predicates, tail) ->
                                                        let term =
                                                            CompoundTerm(functor, arguments, predicates)

                                                        Ok(term, tail |> trim)
                                                    | Error err -> Error err
                                                else if nextToken.value = Comma then
                                                    Ok(compoundTerm, newTail)
                                                else if nextToken.value = RightBracket then
                                                    Ok(compoundTerm, newTail)
                                                else if nextToken.value = RightSquareBracket then
                                                    Ok(compoundTerm, newTail)
                                                else
                                                    Error
                                                        { token = Some nextToken
                                                          message = "compound term should end with ." }
                                            | None -> Ok(compoundTerm, newTail)
                                        else
                                            Error
                                                { token = Some nextToken
                                                  message = "compound term arguments should end with )" }
                                    | None ->
                                        Error
                                            { token = None
                                              message = "compound term arguments should end with )" }
                                | Error err -> Error err
                        | None ->
                            Error
                                { token = None
                                  message = "arguments should end with )" }
                    | _ ->
                        let term = Atom functor
                        Ok(term, tail)
                | None ->
                    Error
                        { token = None
                          message = "term should end with ." }
            | _ ->
                Error
                    { token = Some firstToken
                      message = "term should start with identifier" }
        | None ->
            Error
                { token = None
                  message = "term should start with identifier" }

    let rec parse (clauses: Clause seq) (tokens: Token seq) =
        if Seq.isEmpty tokens then
            Ok clauses
        else
            let token = Seq.head tokens

            match token.value with
            | Identifier _ ->
                match parseTerm tokens with
                | Ok (term, tokens) ->
                    match Seq.tryHead (tokens |> trim) with
                    | Some nextTokne ->
                        if nextTokne.value = Period then
                            match term with
                            | Atom functor ->
                                let clause = { functor = functor; arguments = []; predicates = [] }

                                parse (Seq.append clauses [ clause ]) (Seq.tail tokens |> trim)
                            | CompoundTerm (functor, arguments, predicates) ->
                                if Seq.isEmpty predicates then
                                    let clause =
                                        { functor = functor
                                          arguments = arguments
                                          predicates = [] }

                                    parse (Seq.append clauses [ clause ]) (Seq.tail tokens |> trim)
                                else
                                    let clause =
                                        { functor = functor
                                          arguments = arguments
                                          predicates = predicates }

                                    parse (Seq.append clauses [ clause ]) (Seq.tail tokens |> trim)
                            | _ ->
                                Error
                                    { token = Some token
                                      message = "program should contains valid fact or rule" }
                        else
                            Error
                                { token = None
                                  message = "fact or rule should end with ." }
                    | None ->
                        Error
                            { token = None
                              message = "fact or rule should end with ." }
                | Error err -> Error err
            | Whitespace
            | Newline -> parse clauses (Seq.tail tokens)
            | _ ->
                Error
                    { token = Some token
                      message = "program should contains valid fact or rule" }

module Compiler =
    open Grammar

    type Functor =
        { atom: string
          arity: int32 }

    type Expr =
        { functor: Functor
          arguments: Term array
          predicates: Term array }
