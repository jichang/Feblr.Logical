module Tests.Wam

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Wam

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
