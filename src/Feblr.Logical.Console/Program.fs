open System

open Feblr.Logical.Compiler.Syntax
open Feblr.Logical.Compiler.Grammar

[<EntryPoint>]
let main argv =
    let code =
        """fact(hello, []) :- hello(world), hello(universe, [])."""

    let src =
        { code = code.ToCharArray() |> Array.toSeq
          offset = 0 }

    let result = lex Seq.empty src

    match result with
    | Ok tokens ->
        match parse Seq.empty tokens with
        | Ok clauses -> printfn "tokens: %A" clauses
        | Error err -> printfn "grammar error: %A" err
    | Error err -> printfn "syntax error: %A" err

    0
