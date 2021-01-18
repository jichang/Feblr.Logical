open Feblr.Logical.Compiler.Syntax
open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Wam
open Feblr.Logical.Wam.Machine

[<EntryPoint>]
let main argv =
    let code = "father(don, fsharp)."

    let src =
        { code = code.ToCharArray() |> Array.toSeq
          offset = 0 }

    let result = lex Seq.empty src

    match result with
    | Ok tokens ->
        match parse Seq.empty tokens with
        | Ok terms ->
            match compile List.empty terms with
            | Ok clauses ->
                let machine = Machine.load clauses
                let stack = { layers = [||] }
                let lang = "fsharp"
                let query = CompoundTerm (Atom "father", [Var "Who"; Atom lang])
                let result = Machine.derive machine (Ok stack) query
                match result with
                | Ok stack ->
                    let layer = stack.layers.[0]
                    let binding = List.head layer.bindings
                    match binding with
                    | (Var "Who", Atom who) ->
                        printfn "%A is father of %A" who lang
                    | _ ->
                        printfn "can not find father of fsharp"
                | Error err ->
                    printfn "derive error: %A" err
            | Error err -> printfn "compiler error: %A" err
        | Error err -> printfn "grammar error: %A" err
    | Error err -> printfn "syntax error: %A" err

    0
