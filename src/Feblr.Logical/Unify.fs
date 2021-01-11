module Feblr.Logical.Unify

open Feblr.Logical.Compiler.Grammar

type Binding = Term * Term

let accumulate (bindings: Binding list option) (newBindings: Binding list option) =
    match bindings, newBindings with
    | Some bindings, Some newBindings -> Some(List.append bindings newBindings)
    | _ -> None

let rec robinson (termA: Term) (termB: Term): Binding list option =
    match termA, termB with
    | Var varA, Var varB -> if varA = varB then Some [] else None
    | Var _, _ -> Some [ (termA, termB) ]
    | _, Var _ -> Some [ (termB, termA) ]
    | Str strA, Str strB -> if strA = strB then Some [] else None
    | Num numA, Num numB -> if numA = numB then Some [] else None
    | Atom atomA, Atom atomB -> if atomA = atomB then Some [] else None
    | List listA, List listB ->
        Seq.zip listA listB
        |> Seq.map (fun (termA, termB) -> robinson termA termB)
        |> Seq.fold accumulate (Some [])
    | CompoundTerm (functorA, argumentsA), CompoundTerm (functorB, argumentsB) ->
        if functorA = functorB then
            Seq.zip argumentsA argumentsB
            |> Seq.map (fun (termA, termB) -> robinson termA termB)
            |> Seq.fold accumulate (Some [])

        else
            None
    | _ -> None
