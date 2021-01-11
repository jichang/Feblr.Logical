module Tests

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Syntax
open Feblr.Logical.Compiler.Grammar


type LexerTest(output: ITestOutputHelper) =
    member __.logError(err: SyntaxError) =
        output.WriteLine("syntax error: {0}", err)

    [<Fact>]
    member this.``Lexer should accept valid integer number``() =
        let code = "012123"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Digits digits -> digits |> should equivalent code
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid float number``() =
        let code = "012123.1"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Digits digits -> digits |> should equivalent code
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid term``() =
        let code = "atAom"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Identifier term -> term |> should equivalent code
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid variable``() =
        let code = "Variable_"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Variable variable -> variable |> should equivalent code
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept valid string``() =
        let code = "`Variable_Hello`"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Chars chars ->
                chars
                |> should equivalent (Seq.skip 1 code |> Seq.take (Seq.length code - 2))
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept other identifier``() =
        let code = ":-"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 1)

            let token = Seq.head tokens
            Assert.Equal(token.offset, 0)

            match token.value with
            | Identifier str -> str |> should equivalent code
            | _ -> Assert.True(false)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept special chars``() =
        let code = "[](),. \n"

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens -> Assert.Equal(Seq.length tokens, 8)
        | Error err ->
            this.logError err
            Assert.True(false)

    [<Fact>]
    member this.``Lexer should accept multiple tokens``() =
        let code = """
            124343
            ateomsdf
            Adfsdf
            _adfsdf
            [](),.
            :-
            +
            -
            """


        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens -> Assert.Equal(Seq.length tokens, 130)
        | Error err ->
            this.logError err
            Assert.True(false)

type GrammarTest(output: ITestOutputHelper) =
    member __.logSyntaxError(err: SyntaxError) =
        output.WriteLine("syntax error: {0}", err)

    member __.logGrammarError(err: GrammarError) =
        output.WriteLine("grammar error: {0}", err)

    [<Fact>]
    member this.``Grammer should accept fact without arguments``() =

        let code = """fact."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 2)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")
                Assert.Equal(Seq.length clause.arguments, 0)
                Assert.Equal(Seq.length clause.predicates, 0)
            | Error err ->
                Assert.True(false)
                this.logGrammarError err
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with arguments``() =

        let code = """fact(hello)."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 5)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")
                Assert.Equal(Seq.length clause.arguments, 1)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                Assert.Equal(Seq.length clause.predicates, 0)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with string argument``() =

        let code = """fact(hello, `Hello, world`)."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 8)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")
                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | Str str ->
                    Assert.Equal(str, "Hello, world")
                | _ ->
                    Assert.True(false)
                Assert.Equal(Seq.length clause.predicates, 0)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept fact with list arguments``() =

        let code = """fact(hello, [])."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 9)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")
                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | List list ->
                    Assert.Equal(Seq.length list, 0)
                | _ ->
                    Assert.True(false)
                Assert.Equal(Seq.length clause.predicates, 0)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with arguments``() =
        let code = """fact(hello, []) :- hello(world)."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 16)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | List list ->
                    Assert.Equal(Seq.length list, 0)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 1)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with multiple predicates``() =
        let code =
            """fact(hello, []) :- hello(world), hello(universe, [])."""

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 26)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | List list ->
                    Assert.Equal(Seq.length list, 0)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 2)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "universe")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 0)
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with multiple statements``() =
        let code = """
            fact(hello, []) :- hello(world), hello(universe, []).
            fact(hello2, []) :- hello2(world), hello2(universe, []).
            """

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 91)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 2)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | List list ->
                    Assert.Equal(Seq.length list, 0)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 2)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "universe")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 0)
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)

                let clause = Seq.item 1 clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello2")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | List list ->
                    Assert.Equal(Seq.length list, 0)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 2)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello2")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello2")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "universe")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 0)
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with nested arguments``() =
        let code = """
            fact(hello, fact(hello, [])) :- hello(world), hello(universe, []).
            """

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 58)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "fact")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "hello")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 0)
                    | _ ->
                        Assert.True(false)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 2)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "universe")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 0)
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

    [<Fact>]
    member this.``Grammer should accept rule with list arguments``() =
        let code = """
            fact(hello, fact(hello, [hello])) :- hello(world), hello(universe, [world]).
            """

        let src =
            { code = code.ToCharArray() |> Array.toSeq
              offset = 0 }

        let result = lex Seq.empty src

        match result with
        | Ok tokens ->
            Assert.Equal(Seq.length tokens, 60)

            match parse Seq.empty tokens with
            | Ok clauses ->
                Assert.Equal(Seq.length clauses, 1)
                let clause = Seq.head clauses

                Assert.Equal(clause.functor, "fact")

                Assert.Equal(Seq.length clause.arguments, 2)
                match Seq.head clause.arguments with
                | Atom atom ->
                    Assert.Equal(atom, "hello")
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.arguments with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "fact")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "hello")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 1)
                        match Seq.head list with
                        | Atom atom ->
                            Assert.Equal(atom, "hello")
                        | _ ->
                            Assert.True(false)
                    | _ ->
                        Assert.True(false)
                | _ ->
                    Assert.True(false)

                Assert.Equal(Seq.length clause.predicates, 2)
                match Seq.head clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 1)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "world")
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
                match Seq.item 1 clause.predicates with
                | CompoundTerm (functor, arguments, predicates) ->
                    Assert.Equal(functor, "hello")
                    Assert.Equal(Seq.length arguments, 2)
                    match Seq.head arguments with
                    | Atom atom ->
                        Assert.Equal(atom, "universe")
                    | _ ->
                        Assert.True(false)
                    match Seq.item 1 arguments with
                    | List list ->
                        Assert.Equal(Seq.length list, 1)
                        match Seq.head list with
                        | Atom atom ->
                            Assert.Equal(atom, "world")
                        | _ ->
                            Assert.True(false)
                    | _ ->
                        Assert.True(false)
                    Assert.Equal(Seq.length predicates, 0)
                | _ ->
                    Assert.True(false)
            | Error err ->
                this.logGrammarError err
                Assert.True(false)
        | Error err ->
            this.logSyntaxError err
            Assert.True(false)

type CompilerTest(output: ITestOutputHelper) =
    member __.logSyntaxError(err: SyntaxError) =
        output.WriteLine("syntax error: {0}", err)

    member __.logGrammarError(err: GrammarError) =
        output.WriteLine("grammar error: {0}", err)
