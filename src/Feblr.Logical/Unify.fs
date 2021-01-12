module Feblr.Logical.Unify

open Feblr.Logical.Compiler.Grammar

type Binding = Term * Term

type UnifyError =
    { source: Term
      target: Term }

type UnifyResult = Result<Binding list, UnifyError>

let accumulate (bindings: UnifyResult) (newBindings: UnifyResult): UnifyResult =
    match bindings, newBindings with
    | Ok bindings, Ok newBindings -> Ok(List.append bindings newBindings)
    | Ok _, Error err -> Error err
    | Error err, _ -> Error err

let rec robinson (termA: Term) (termB: Term): UnifyResult =
    match termA, termB with
    | Var varA, Var varB -> if varA = varB then Ok [] else Ok [(termA, termB)]
    | Var _, _ -> Ok [ (termA, termB) ]
    | _, Var _ -> Ok [ (termB, termA) ]
    | Str strA, Str strB -> if strA = strB then Ok [] else Error { source = termA; target = termB }
    | Num numA, Num numB -> if numA = numB then Ok [] else Error { source = termA; target = termB }
    | Atom atomA, Atom atomB -> if atomA = atomB then Ok [] else Error { source = termA; target = termB }
    | List listA, List listB ->
        if Seq.length listA = Seq.length listB then
            Seq.zip listA listB
            |> Seq.map (fun (termA, termB) -> robinson termA termB)
            |> Seq.fold accumulate (Ok [])
        else
            Error { source = termA; target = termB }
    | CompoundTerm (functorA, argumentsA), CompoundTerm (functorB, argumentsB) ->
        if functorA = functorB then
            if Seq.length argumentsA = Seq.length argumentsB then
                Seq.zip argumentsA argumentsB
                |> Seq.map (fun (termA, termB) -> robinson termA termB)
                |> Seq.fold accumulate (Ok [])
            else
                Error { source = termA; target = termB }
        else
            Error { source = termA; target = termB }
    | _ -> Error { source = termA; target = termB }
