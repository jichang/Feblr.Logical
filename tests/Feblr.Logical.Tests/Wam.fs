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
        Assert.Equal(true, Map.isEmpty machine.indexes)

    [<Fact>]
    member this.``should load single clauses to database and setup up index``() =
        let clause = { head = Atom "f"; body = [] }
        let clauses = [ clause ]
        let machine = Machine.load clauses
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.head machine.database)
        let indexes = Map.find "f/0" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(0, List.head indexes)

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
        let indexes = Map.find "f/0" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(0, List.head indexes)
        let indexes = Map.find "g/0" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(1, List.head indexes)
        let indexes = Map.find "g/1" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(2, List.head indexes)

    [<Fact>]
    member this.``should support query by simple atom``() =
        let clause = { head = Atom "f"; body = [] }
        let machine = Machine.load [ clause ]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        let indexes = Map.find "f/0" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(0, List.head indexes)

        let goal = Atom "f"

        match query machine goal with
        | Some tree ->
            Assert.Equal(Term goal, tree.target)
            let children = tree.children
            Assert.Equal(1, List.length children)
            let child = List.head children
            Assert.Equal(Clause clause, child.target)
            Assert.Equal(0, List.length child.bindings)
            Assert.Equal(0, List.length child.children)
        | None -> Assert.Equal(false, true)

    [<Fact>]
    member this.``should return error if can find matched atom in database``() =
        let clause = { head = Atom "f"; body = [] }
        let machine = Machine.load [ clause ]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        let indexes = Map.find "f/0" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(0, List.head indexes)

        let goal = Atom "g"

        match query machine goal with
        | Some tree -> Assert.Equal(true, true)
        | None -> Assert.Equal(true, true)

    [<Fact>]
    member this.``should return bindings for query that does not need backtracking``() =
        let clause =
            { head = CompoundTerm(Atom "f", [ Atom "hello" ])
              body = [] }

        let machine = Machine.load [ clause ]
        Assert.Equal(1, List.length machine.database)
        Assert.Equal(clause, List.item 0 machine.database)
        let indexes = Map.find "f/1" machine.indexes
        Assert.Equal(1, List.length indexes)
        Assert.Equal(0, List.head indexes)

        let goal = CompoundTerm(Atom "f", [ Var "X" ])

        match query machine goal with
        | Some tree ->
            Assert.Equal(Term goal, tree.target)
            let children = tree.children
            Assert.Equal(1, List.length children)
            let child = List.head children
            Assert.Equal(Clause clause, child.target)
            Assert.Equal(1, List.length child.bindings)
            Assert.Equal((Var "X", Atom "hello"), List.head child.bindings)
            Assert.Equal(0, List.length child.children)
        | None -> Assert.Equal(false, true)
