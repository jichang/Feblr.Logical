namespace Feblr.Prolog.Compiler

module Syntax =
    type Source =
        { code: char seq
          offset: int32 }

    type TokenValue =
        | Num of char seq
        | Identifier of char seq
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
        | IdentifierMarker
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
            NumMarker
        else if isLowerCase c then
            IdentifierMarker
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

    type SyntaxError =
        { offset: int32
          expected: string
          actual: string }

    let parseNum (source: Source) =
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

    let parseIdentifier (source: Source) =
        let identifier =
            Seq.takeWhile (fun c ->isAlphabet c || isDigit c) source.code
        let length = Seq.length identifier
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar || isPeriod nextChar || isLeftBracket nextChar || isRightBracket nextChar || isComma nextChar || isRightSquareBracket nextChar then
                Ok ({ value = Identifier identifier; offset = source.offset }, length)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Identifier identifier; offset = source.offset }, length)

    let parseVariable (source: Source) =
        let variable =
            Seq.takeWhile (fun c -> isAlphabet c || isUnderscore c || isDigit c)source.code
        let length = Seq.length variable
        match Seq.tryItem length source.code with
        | Some nextChar ->
            if isWhitespace nextChar || isNewline nextChar || isPeriod nextChar then
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
            if isWhitespace nextChar || isNewline nextChar|| isPeriod nextChar || isRightBracket nextChar || isComma nextChar || isRightSquareBracket nextChar then
                Ok ({ value = Str str; offset = source.offset }, length + 2)
            else
                Error { offset = source.offset + length; expected = "whitespace or newline"; actual = $"{nextChar}" }
        | None ->
            Ok ({ value = Str str; offset = source.offset }, length + 2)

    let parseOther (source: Source) =
        let identifier =
            Seq.takeWhile (fun c -> not (isWhitespace c || isNewline c)) source.code
        Ok ({ value = Identifier identifier; offset = source.offset }, Seq.length identifier)

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

module rec Grammar =
    open Syntax

    type Argument =
        | Value of Token
        | Subterm of Predicate
        | List of Token seq
    and Predicate =
        { functor: Token
          arguments: Argument seq }

    type Ast =
        | Fact of Predicate
        | Rule of Rule
    and Rule =
        { fact: Predicate
          predicates: Ast seq }

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

    let rec parseList (items: Token seq) (tokens: Token seq) =
        match Seq.tryHead tokens with
        | Some firstToken ->
            match firstToken.value with
            | RightSquareBracket ->
                Ok (items, Seq.tail tokens)
            | Identifier _
            | Variable _
            | Num _
            | Str _ ->
                let tail = Seq.tail tokens |> trim
                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | Comma ->
                        parseList (Seq.append items [firstToken]) (Seq.tail tail)
                    | RightSquareBracket ->
                        Ok (items, Seq.tail tail)
                    | _ ->
                        Error { token = Some nextToken; message = "list should end with ] or item in list should separated with ," }
                | None ->
                    Error { token = None; message = "list should end with ]"}
            | _ ->
                Error { token = Some firstToken; message = "list should start with term or end with ]" }
        | None ->
            Error { token = None; message = "list should start with term or end with ]" }

    let rec parseArguments (arguments: Argument seq) (tokens: Token seq) =
        match Seq.tryHead tokens with
        | Some firstToken ->
            match firstToken.value with
            | RightBracket ->
                Ok (arguments, Seq.tail tokens)
            | LeftSquareBracket ->
                match parseList (Seq.empty) (Seq.tail tokens |> trim) with
                | Ok (items, tail) ->
                    match Seq.tryHead (trim tail) with
                    | Some nextToken ->
                        let argument = List items
                        match nextToken.value with
                        | Comma ->
                            parseArguments (Seq.append arguments [argument]) (trim tail |> Seq.tail)
                        | RightBracket ->
                            Ok (Seq.append arguments [argument], Seq.tail tail)
                        | _ ->
                            Error { token = Some nextToken; message = "arguments should end with )" }
                    | None ->
                        Error { token = None; message = "arguments should end with )" }                        
                | Error err ->
                    Error err
            | Identifier token ->
                let tail = Seq.tail tokens |> trim
                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | Comma ->
                        let argument = Value firstToken
                        parseArguments (Seq.append arguments [argument]) (Seq.tail tail |> trim)
                    | RightBracket ->
                        let argument = Value firstToken
                        Ok (Seq.append arguments [argument], Seq.tail tail)
                    | LeftBracket ->
                        match parseFact tokens with
                        | Ok (fact, rest) ->
                            let argument = Subterm fact
                            parseArguments (Seq.append arguments [argument]) (rest |> trim)
                        | Error err ->
                            Error err
                    | _ ->
                        Error { token = Some nextToken; message = "arguments should end with ) or item in list should be separated by ," }                    
                | None ->
                    Error { token = Some firstToken; message = "arguments should end with ) or item in list should be separated by ," }
            | Variable token
            | Num token
            | Str token ->
                let tail = Seq.tail tokens |> trim
                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | Comma ->
                        let argument = Value firstToken
                        parseArguments (Seq.append arguments [argument]) (Seq.tail tail |> trim)
                    | RightBracket ->
                        let argument = Value firstToken
                        Ok (Seq.append arguments [argument], Seq.tail tail)
                    | _ ->
                        Error { token = Some nextToken; message = "arguments should end with ) or item in list should be separated by ," }
                | None ->
                    Error { token = Some firstToken; message = "arguments should end with ) or item in list should be separated by ," }
            | _ ->
                Error { token = Some firstToken; message = "arguments should start with term or end with )" }
        | None ->
            Error { token = None; message = "arguments should start with term or end with )" }

    let parseFact (tokens: Token seq) =
        match Seq.tryHead tokens with
        | Some firstToken ->
            match firstToken.value with
            | Identifier _ ->
                let tail = Seq.tail tokens |> trim
                match Seq.tryHead tail with
                | Some nextToken ->
                    match nextToken.value with
                    | LeftBracket ->
                        match parseArguments Seq.empty (tail |> Seq.tail) with
                        | Ok (arguments, tokens) ->
                            Ok ({ functor = firstToken; arguments = arguments }, tokens)
                        | Error err ->
                            Error err
                    | Comma
                    | Period ->
                        Ok ({ functor = nextToken; arguments = [] }, tail |> Seq.tail)
                    | _ ->
                        Error { token = Some nextToken; message = "fact should be separated by , or end with ." }
                | None ->
                    Error { token = None; message = "fact should be separated by , or end with ." }
            | _ ->
                Error { token = Some firstToken; message = "fact should start with identifier" }
        | None ->
            Error { token = None; message = "fact should start with identifier" }

    let rec parsePredicates (clauses: Ast seq) (tokens: Token seq) =
        match parseFact (trim tokens) with
        | Ok (fact, tokens) ->
            let tail = trim tokens
            match Seq.tryHead tail with
            | Some nextToken ->
                match nextToken.value with
                | Period ->
                    Ok (Seq.append clauses [Fact fact], Seq.tail tail |> trim)
                | Comma ->
                    parsePredicates (Seq.append clauses [Fact fact]) (Seq.tail tail |> trim)
                | _ ->
                    Error { token = Some nextToken; message = "predicate should end with period . or separated with ,"}
            | None ->
                Error { token = None; message = "predicate should end with period"}
        | Error err ->
            Error err

    let rec parse (asts: Ast seq) (tokens: Token seq) =
        if Seq.isEmpty tokens then
            Ok asts
        else
            let token = Seq.head tokens
            match token.value with
            | Identifier _ ->
                match parseFact tokens with
                | Ok (fact, tokens) ->
                    let rest = trim tokens
                    match Seq.tryHead rest with
                    | Some nextToken ->
                        match nextToken.value with
                        | Period ->
                            let fact = Fact fact
                            parse (Seq.append asts [fact]) (Seq.tail rest)
                        | Identifier identifier ->
                            if Seq.compareWith compare identifier [':';'-'] = 0 then
                                match parsePredicates Seq.empty (Seq.tail rest) with
                                | Ok (predicates, tokens) ->
                                    let rule = Rule { fact = fact; predicates = predicates }
                                    parse (Seq.append asts [rule]) tokens
                                | Error err ->
                                    Error err
                            else
                                Error { token = Some nextToken; message = "rules should start with :-"}
                        | _ ->
                            Error { token = Some nextToken; message = "fact should end with period . or rules should start with :-"}
                    | None ->
                        Ok (Seq.append asts [Fact fact])
                | Error err ->
                    Error err
            | Whitespace
            | Newline ->
                parse asts (Seq.tail tokens)
            | _ ->
                Error { token = Some token; message = "program should start with an fact or rule" }

module Compiler =
    let compile = ()
