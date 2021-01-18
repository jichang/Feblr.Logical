module Tests.Wam

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Wam
open Feblr.Logical.Wam.Machine

type MachineTest(output: ITestOutputHelper) =
    [<Fact>]
    member this.``should load clauses to database and setup up index``() =
        let clauses = List.empty
        let machine = Machine.load clauses
        Assert.Equal(0, List.length machine.database)
        Assert.Equal(true, Map.isEmpty machine.index)

    [<Fact>]
    member this.``should load single clauses to database and setup up index``() =
        let clause = { head = Atom "f"; body = [] }
        let clauses = [ clause ]
        let machine = Machine.load clauses
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.head machine.database)
        Assert.Equal(0, Map.find "f/0" machine.index)

    [<Fact>]
    member this.``should load multiple clauses to database and setup up index``() =
        let clause = { head = Atom "f"; body = [] }
        let clause2 = { head = Atom "g"; body = [] }

        let clause3 =
            { head = CompoundTerm(Atom "g", [ Var "X" ])
              body = [] }

        let clauses = [ clause; clause2; clause3 ]
        let machine = Machine.load clauses
        Assert.Equal(3, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        Assert.Equal(clause2, List.item 1 machine.database)
        Assert.Equal(clause3, List.item 2 machine.database)
        Assert.Equal(0, Map.find "f/0" machine.index)
        Assert.Equal(1, Map.find "g/0" machine.index)
        Assert.Equal(2, Map.find "g/1" machine.index)

    [<Fact>]
    member this.``should support query by simple atom``() =
        let clause = { head = Atom "f"; body = [] }
        let machine = Machine.load [clause]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        Assert.Equal(0, Map.find "f/0" machine.index)

        let query = Atom "f"
        let stack = { layers = [||] }
        let result = Machine.derive machine (Ok stack) query
        match result with
        | Ok stack ->
            Assert.Equal(0, Array.length stack.layers)
        | Error error ->
            Assert.Equal(false, true)

    [<Fact>]
    member this.``should return error if can find matched atom in database``() =
        let clause = { head = Atom "f"; body = [] }
        let machine = Machine.load [clause]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        Assert.Equal(0, Map.find "f/0" machine.index)

        let query = Atom "g"
        let stack = { layers = [||] }
        let result = Machine.derive machine (Ok stack) query
        match result with
        | Ok stack ->
            Assert.Equal(false, true)
        | Error (Match error) ->
            Assert.Equal(query, error.query)
            Assert.Equal("no matched clause found in database", error.message)
        | Error _ ->
            Assert.Equal(false, true)

    [<Fact>]
    member this.``should return bindings for query that does not need backtracking``() =
        let clause = { head = CompoundTerm (Atom "f", [Atom "hello"]); body = [] }
        let machine = Machine.load [clause]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        Assert.Equal(0, Map.find "f/1" machine.index)

        let query = CompoundTerm (Atom "f", [Var "X"])
        let stack = { layers = [||] }
        let result = Machine.derive machine (Ok stack) query
        match result with
        | Ok stack ->
            Assert.Equal(1, Array.length stack.layers)
            let layer = stack.layers.[0]
            Assert.Equal(1, List.length layer.bindings)
            let binding = List.head layer.bindings
            Assert.Equal((Var "X", Atom "hello"), binding)
        | Error _ ->
            Assert.Equal(false, true)
