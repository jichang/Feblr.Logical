module Tests.Unify

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Unify

type OccoursTest(output: ITestOutputHelper) =
    [<Fact>]
    member this.``Occurs should not return matched term when Variable as functor``() =
        let variable = Var "X"
        let term = CompoundTerm (Var "Y", [])
        match occurs variable term with
        | Some term ->
            Assert.Equal(false, true)
        | None ->
            Assert.Equal(true, true)

    [<Fact>]
    member this.``Occurs should return not matched term when Variable as argument (which should never happend``() =
        let variable = Var "X"
        let term = CompoundTerm (Var "Y", [Var "Z"])
        match occurs variable term with
        | Some term ->
            Assert.Equal(false, true)
        | None ->
            Assert.Equal(true, true)

    [<Fact>]
    member this.``Occurs should return matched term when Variable as functor (which should never happend``() =
        let variable = Var "X"
        let term = CompoundTerm (variable, [])
        match occurs variable term with
        | Some term ->
            Assert.Equal(variable, term)
        | None ->
            Assert.Equal(false, true)

    [<Fact>]
    member this.``Occurs should return matched term when Variable as argument (which should never happend``() =
        let variable = Var "X"
        let term = CompoundTerm (Var "Y", [variable])
        match occurs variable term with
        | Some term ->
            Assert.Equal(variable, term)
        | None ->
            Assert.Equal(false, true)


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
        let source = Var "X"
        let target = Var "X"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | Error err ->
            this.logError(err)
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify different variables with one bindings``() =
        let source = Var "X"
        let target = Var "Y"
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((source, target), List.head bindings)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should unify variable with another atom``() =
        let source = Var "X"
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
        let source = Var "X"
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
        let source = Var "X"
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
        let source = Var "X"
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
        let source = CompoundTerm (Atom "f", [])
        let target = CompoundTerm (Atom "f", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(0, List.length bindings)
        | _ ->
            Assert.Equal(true, false)

    [<Fact>]
    member this.``Unify should not unify compound terms with differnt arity``() =
        let source = CompoundTerm (Atom "f", [Atom "hello"])
        let target = CompoundTerm (Atom "f", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(true, false)
        | Error err ->
            Assert.Equal(true, true)

    [<Fact>]
    member this.``Unify should not unify compound terms with compatible arguments``() =
        let source = CompoundTerm (Atom "f", [Atom "a"; Var "X"])
        let target = CompoundTerm (Atom "f", [Atom "a"; Atom "b"])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((Var "X", Atom "b"), List.head bindings)
        | Error err ->
            this.logError err
            Assert.Equal(true, false)
            
    [<Fact>]
    member this.``Unify should not unify compound terms with different functor``() =
        let source = CompoundTerm (Atom "f", [])
        let target = CompoundTerm (Atom "g", [])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(true, false)
        | _ ->
            Assert.Equal(true, true)
            
    [<Fact>]
    member this.``Unify should not unify compound terms with one compatible arguments``() =
        let source = CompoundTerm (Atom "f", [Num "1"; Num "2"])
        let target = CompoundTerm (Atom "f", [Num "1"; Var "Y"])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(1, List.length bindings)
            Assert.Equal((Var "Y", Num "2"), List.head bindings)
        | _ ->
            Assert.Equal(true, false)
            
    [<Fact>]
    member this.``Unify should not unify compound terms with multiple compatible arguments``() =
        let source = CompoundTerm (Atom "f", [Var "X"; Num "2"])
        let target = CompoundTerm (Atom "f", [Num "1"; Var "Y"])
        match robinson source target with
        | Ok bindings ->
            Assert.Equal(2, List.length bindings)
            Assert.Equal((Var "X", Num "1"), List.head bindings)
            Assert.Equal((Var "Y", Num "2"), List.item 1 bindings)
        | _ ->
            Assert.Equal(true, false)
