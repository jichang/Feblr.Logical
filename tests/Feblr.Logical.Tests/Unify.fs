module Tests

open Xunit
open Xunit.Abstractions
open FsUnit

open Feblr.Logical.Compiler.Syntax
open Feblr.Logical.Compiler.Grammar


type UnifyTest(output: ITestOutputHelper) =
    member __.logError(err: SyntaxError) =
        output.WriteLine("syntax error: {0}", err)

    [<Fact>]
    member this.``Unify should accept valid integer number``() =
        ()