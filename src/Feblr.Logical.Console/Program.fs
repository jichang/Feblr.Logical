open Feblr.Logical.Compiler.Syntax
open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Wam
open Feblr.Logical.Wam.Machine

let compileCode (code: string) =
    let src =
        { code = code.ToCharArray() |> Array.toSeq
          offset = 0 }

    let result = lex Seq.empty src

    match result with
    | Ok tokens ->
        match parse Seq.empty tokens with
        | Ok terms ->
            match compile List.empty terms with
            | Ok clauses -> Some clauses
            | Error err ->
                printfn "compiler error: %A" err
                None
        | Error err ->
            printfn "grammar error: %A" err
            None
    | Error err ->
        printfn "syntax error: %A" err
        None

[<EntryPoint>]
let main argv =
    let code = "
        father(a, b).
        father(b, c).
        grandfather(X, Z) :- father(X, Y), father(Y, Z).
    "

    match compileCode code with
    | Some clauses ->
        let machine = Machine.load clauses
        let goal = "grandfather(a, X)."

        match compileCode goal with
        | Some clauses ->
            let clause = List.head clauses

            match query machine clause.head with
            | Some tree -> printfn "%A" tree
            | None -> printfn "can not satisfy goal"
        | None -> printfn "can not compile goal"
    | None -> printfn "can not compile code"

    0
