namespace Feblr.Prolog.Wam

module Lexer =
    type Source =
        { code: char seq
          offset: int32 }

    type Token =
        | Num of char seq
        | Term of char seq
        | Variable of char seq
        | Str of char seq
        | LeftSquareBracket
        | RightSquareBracket
        | LeftBraket
        | RightBracket
        | Comma
        | Period
        | Whitespace
        | Newline

    type Marker =
        | Num
        | Term
        | Variable
        | Str
        | LeftSquareBracket
        | RightSquareBracket
        | LeftBraket
        | RightBracket
        | Comma
        | Period
        | Whitespace
        | Newline
        | Other

    let checkMark (c: char): Marker =
        if c >= '0' && c <= '9' then
            Num
        else if c >= 'a' && c <= 'z' then
            Term
        else if (c >= 'A' && c <= 'Z') || c = '_' then
            Variable
        else if c = '"' then
            Str
        else if c = '[' then
            LeftSquareBracket
        else if c = ']' then
            RightSquareBracket
        else if c = '(' then
            LeftBraket
        else if c = ')' then
            RightBracket
        else if c = ',' then
            Comma
        else if c = '.' then
            Period
        else if c = ' ' then
            Whitespace
        else if c = '\n' then
            Newline
        else
            Other

    let parseNum (source: Source) =
        let num =
            Seq.takeWhile (fun c -> (c >= '0' && c <= '9') || c = '.') source.code
        num

    let rec lex (tokens: Token seq) (source: Source) =
        let c = Seq.head source.code
        match checkMark c with
        | Num
        | Term
        | Variable
        | Str
        | LeftSquareBracket
        | RightSquareBracket
        | LeftBraket
        | RightBracket
        | Comma
        | Period
        | Whitespace
        | Newline
        | Other -> Ok tokens

module Parser =
    let parse = ()

module Compiler =
    let compile = ()
