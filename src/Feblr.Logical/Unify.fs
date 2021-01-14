module Feblr.Logical.Unify

open Feblr.Logical.Compiler.Grammar

type Binding = Term * Term

type UnifyError =
    { source: Term
      target: Term }

type UnifyResult = Result<Binding list, UnifyError>

let rec occurs (termA: Term) (termB: Term) =
    match termA, termB with
    | Var x, CompoundTerm (f, arguments) ->
        if termA = f then
            Some (f)
        else
            match List.tryFind (fun term -> occurs termA term |> Option.isSome) arguments with
            | Some term ->
                Some term
            | _ ->
                None
    | Var x, Var y ->
        if x = y then
            Some termB
        else
            None
    | _ ->
        None

let accumulate (bindings: UnifyResult) (newBindings: UnifyResult): UnifyResult =
    match bindings, newBindings with
    | Ok bindings, Ok newBindings -> Ok(List.append bindings newBindings)
    | Ok _, Error err -> Error err
    | Error err, _ -> Error err

let rec robinson (termA: Term) (termB: Term): UnifyResult =
    match termA, termB with
    | Var varA, Var varB -> if varA = varB then Ok [] else Ok [(termA, termB)]
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
    | Var _, Num _
    | Var _, Str _
    | Var _, List _
    | Var _, Atom _
    | Var _, Oper _ ->
        Ok [ (termA, termB )]
    | Var _, CompoundTerm _ ->
        match occurs termA termB with
        | Some term ->
            Error { source = termA; target = term }
        | None ->
            Ok [ (termA, termB) ]
    | Num _, Var _
    | Str _, Var _
    | List _, Var _
    | Atom _, Var _
    | Oper _, Var _ ->
        Ok [(termB, termA )]
    | CompoundTerm _, Var _ ->
        match occurs termB termA with
        | Some term ->
            Error { source = termB; target = term }
        | None ->
            Ok [ (termA, termB) ]
    | _ ->
        Error { source = termA; target = termB }
