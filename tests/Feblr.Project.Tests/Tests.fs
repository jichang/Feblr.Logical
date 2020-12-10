module Tests

open Xunit
open Xunit.Abstractions;
open FsUnit

open Feblr.Prolog.Compiler.Lexer


type LexerTest(output: ITestOutputHelper) =
    member __.logError (err: LexerError) =
        output.WriteLine("offset: {0}", err.offset)
        output.WriteLine("expected: {0}", err.expected)
        output.WriteLine("actual: {0}", int(err.actual.[0]))

    [<Fact>]
    member this.``Lexer should accept valid integer number`` () =
        let code = "012123".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Num num ->
                num |> should equivalent code
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid float number`` () =
        let code = "012123.1".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Num num ->
                num |> should equivalent code
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid term`` () =
        let code = "atom".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Atom term ->
                term |> should equivalent code
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid variable`` () =
        let code = "Variable_".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Variable variable ->
                variable |> should equivalent code
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid string`` () =
        let code = "`Variable_Hello`".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Str str ->
                str |> should equivalent (Seq.skip 1 code |> Seq.take (Seq.length code - 2))
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept other atom`` () =
        let code = ":-".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0);

            match token.value with
            | Atom str ->
                str |> should equivalent code
            | _ ->
                Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept special chars`` () =
        let code = "[](),. \n".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 8)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept multiple tokens`` () =
        let code =
            """
            124343
            ateomsdf
            Adfsdf
            _adfsdf
            [](),.
            :-
            +
            -
            """.ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 139)
        | Error err ->
            this.logError err
            Assert.True(false)
