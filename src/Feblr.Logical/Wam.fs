namespace Feblr.Logical.Wam

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Unify

module Machine =
    type Machine =
        { index: Map<string, int32>
          database: Clause list }

        static member empty =
            { index = Map.empty
              database = List.empty }

        static member append (machine: Machine) (clause: Clause) =
            let key =
                match clause.head with
                | Atom atom -> Some $"{atom}/0"
                | CompoundTerm (Atom atom, arguments) -> Some $"{atom}/{List.length arguments}"
                | _ -> None

            match key with
            | Some key ->
                let database = List.append machine.database [ clause ]

                let index =
                    Map.add key (Seq.length database - 1) machine.index

                { index = index; database = database }
            | None -> machine

    let load (clauses: Clause list) =
        List.fold Machine.append Machine.empty clauses

    type Query = Term

    type Layer = {  bindings: Binding list }

    type DeriveStack = { layers: Layer array }

    type DatabaseError = { query: Query; message: string }

    type DeriveError =
        | Match of DatabaseError
        | Unify of UnifyError

    let unify (machine: Machine) (stack: Result<DeriveStack, DeriveError>) ((termA, termB): Term * Term) =
        match stack with
        | Ok stack ->
            let result = robinson termA termB

            match result with
            | Ok bindings ->
                let layer = { bindings = bindings }
                Ok { layers = Array.append stack.layers [|layer|] }
            | Error error -> Error(Unify error)
        | Error error -> Error error

    let rec derive (machine: Machine) (stack: Result<DeriveStack, DeriveError>) (query: Query) =
        match stack with
        | Ok _ ->
            match query with
            | Atom atom ->
                let key = $"{atom}/0"

                match Map.tryFind key machine.index with
                | Some index ->
                    let clause = Seq.item index machine.database
                    List.fold (derive machine) stack clause.body
                | None ->
                    let error =
                        { query = query
                          message = "no matched clause found in database" }

                    Error(Match error)
            | CompoundTerm (Atom atom, arguments) ->
                let key = $"{atom}/{List.length arguments}"

                match Map.tryFind key machine.index with
                | Some index ->
                    let clause = Seq.item index machine.database
                    match clause.head with
                    | CompoundTerm (atom, _arguments) ->
                        let deriveArgsStack =
                            List.zip arguments _arguments
                            |> List.fold (unify machine) stack

                        match deriveArgsStack with
                        | Ok _ -> List.fold (derive machine) deriveArgsStack clause.body
                        | Error error -> Error error
                    | _ ->
                        Error (Match { query = query; message = "compound term can't match to other kind of term"})
                | None ->
                    let error =
                        { query = query
                          message = "no matched clause found in database" }

                    Error(Match error)
            | _ ->
                let error =
                    { query = query
                      message = "query need to be atom or compound term" }

                Error(Match error)
        | Error error -> Error error
