module Tests.Unify

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Unify

type UnifyTest(output: ITestOutputHelper) =
    member __.logError(err: UnifyError) =
        output.WriteLine("unify error: {0}", err)

    [<Fact>]
    member this.``Unify should unify same atoms``() =
        let source = Atom "a"
        let target = Atom "a"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)

    [<Fact>]
    member this.``Unify should not unify different atoms``() =
        let source = Atom "a"
        let target = Atom "b"
        match robinson source target with
        | Error err ->
            Assert.Equal(true, true)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify same number``() =
        let source = Num "0"
        let target = Num "0"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)

    [<Fact>]
    member this.``Unify should not unify different number``() =
        let source = Num "0"
        let target = Num "1"
        match robinson source target with
        | Error err ->
            Assert.Equal(source, err.source)
            Assert.Equal(target, err.target)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify same string``() =
        let source = Str "0"
        let target = Str "0"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)

    [<Fact>]
    member this.``Unify should not unify different string``() =
        let source = Str "0"
        let target = Str "1"
        match robinson source target with
        | Error err ->
            Assert.Equal(source, err.source)
            Assert.Equal(target, err.target)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify same list``() =
        let source = List [Atom "0"]
        let target = List [Atom "0"]
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)

    [<Fact>]
    member this.``Unify should not unify different list``() =
        let source = List [Atom "0"]
        let target = List [Atom "1"]
        match robinson source target with
        | Error err ->
            Assert.Equal(true, true)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify same variable with empty bindings``() =
        let source = Var "x"
        let target = Var "x"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify different variables with one bindings``() =
        let source = Var "x"
        let target = Var "y"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify variable with another atom``() =
        let source = Var "x"
        let target = Atom "x"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
        match robinson target source with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)


    [<Fact>]
    member this.``Unify should unify variable with another string``() =
        let source = Var "x"
        let target = Str "xyxdfs"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
        match robinson target source with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
            
    [<Fact>]
    member this.``Unify should unify variable with another number``() =
        let source = Var "x"
        let target = Num "100000.0"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
        match robinson target source with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
            
    [<Fact>]
    member this.``Unify should unify variable with another list``() =
        let source = Var "x"
        let target = List [Atom "atom"]
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
        match robinson target source with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify same compound terms``() =
        let source = CompoundTerm ("source", [])
        let target = CompoundTerm ("source", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should not unify compound terms with differnt arity``() =
        let source = CompoundTerm ("source", [Atom "hello"])
        let target = CompoundTerm ("source", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(true, false)
        | Error err ->
            Assert.Equal(true, true)
            
    [<Fact>]
    member this.``Unify should not unify compound terms with different functor``() =
        let source = CompoundTerm ("source", [])
        let target = CompoundTerm ("target", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(true, false)
        | _ ->
            Assert.Equal(true, true)