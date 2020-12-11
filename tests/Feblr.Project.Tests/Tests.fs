module Tests

open Xunit
open Xunit.Abstractions;
open FsUnit

open Feblr.Prolog.Compiler.Syntax
open Feblr.Prolog.Compiler.Grammar


type LexerTest(output: ITestOutputHelper) =
    member __.logError (err: SyntaxError) =
        output.WriteLine("offset: {0}", err.offset)
        output.WriteLine("expected: {0}", err.expected)
        output.WriteLine("actual: {0}", err.actual)

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
        let code = "atAom".ToCharArray() |> Array.toSeq
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
            | Identifier term ->
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
    member this.``Lexer should accept other identifier`` () =
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
            | Identifier str ->
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

type GrammarTest(output: ITestOutputHelper) =
    member __.logSyntaxError (err: SyntaxError) =
        output.WriteLine("offset: {0}", err.offset)
        output.WriteLine("expected: {0}", err.expected)
        output.WriteLine("actual: {0}", err.actual)

    member __.logGrammarError (err: GrammarError) =
        output.WriteLine("token: {0}", err.token)
        output.WriteLine("message: {0}", err.message)

    [<Fact>]
    member this.``Grammer should accept fact without arguments`` () =
        
        let code =
            """fact.""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 2)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Fact fact ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                Assert.True(false)
                this.logGrammarError err
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with arguments`` () =
        
        let code =
            """fact(hello).""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 5)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Fact fact ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with string argument`` () =
        
        let code =
            """fact(hello, `Hello, world`).""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 8)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Fact fact ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with list arguments`` () =
        
        let code =
            """fact(hello, []).""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 9)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Fact fact ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)
            
    [<Fact>]
    member this.``Grammer should accept rule with arguments`` () =
        let code =
            """fact(hello, []) :- hello(world).""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 16)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Rule rule ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with multiple predicates`` () =
        let code =
            """fact(hello, []) :- hello(world), hello(universe, []).""".ToCharArray() |> Array.toSeq
        let src =
            { code = code
              offset = 0 }
        let result = lex Seq.empty src
        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 26)
            match parse Seq.empty tokens with
            | Ok asts ->
                Assert.Equal(Seq.length asts, 1)
                let ast = Seq.head asts
                match ast with
                | Rule rule ->
                    Assert.True(true)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)