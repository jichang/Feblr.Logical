namespace Feblr.Logical.Wam

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Unify
open System.Collections.Generic

module Machine =
    type Machine =
        { indexes: Map<string, int32 list>
          database: Clause list }

        static member empty =
            { indexes = Map.empty
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

                let index = List.length database - 1

                let indexes =
                    match Map.tryFind key machine.indexes with
                    | Some indexes -> Map.add key (List.append indexes [ index ]) machine.indexes
                    | None -> Map.add key [ index ] machine.indexes

                { indexes = indexes
                  database = database }
            | None -> machine

    let load (clauses: Clause list) =
        List.fold Machine.append Machine.empty clauses

    type Goal = Term

    type Frame =
        { clause: Clause
          bindings: Binding list }

    type DeriveStack = Stack<Frame>

    type SearchError = { goal: Goal; message: string }

    type DeriveError =
        | Search of SearchError
        | Unify of UnifyError

    let search (machine: Machine) (goal: Term) =
        match goal with
        | Atom atom ->
            let key = $"{atom}/0"

            match Map.tryFind key machine.indexes with
            | Some indexes ->
                let clauses =
                    List.map (fun index -> Seq.item index machine.database) indexes

                Ok(clauses)
            | None ->
                let error =
                    { goal = goal
                      message = "no clause match the goal in database" }

                Error error
        | CompoundTerm (Atom atom, arguments) ->
            let key = $"{atom}/{List.length arguments}"

            match Map.tryFind key machine.indexes with
            | Some indexes ->
                let clauses =
                    List.map (fun index -> Seq.item index machine.database) indexes

                Ok(clauses)
            | None ->
                let error =
                    { goal = goal
                      message = "no clause match the goal in database" }

                Error error
        | _ ->
            let error =
                { goal = goal
                  message = "goal need to be atom or compound term" }

            Error error


    let unify (termA: Term) (termB: Term) =
        let result = robinson termA termB

        match result with
        | Ok bindings ->
            Ok bindings
        | Error error -> Error(Unify error)

    let rec derive (machine: Machine) (clauseStack: Stack<Clause list>) (deriveStack: DeriveStack) (goal: Goal) =
        if clauseStack.Count = 0 then
            Ok deriveStack
        else
            let clauses = clauseStack.Peek()
            if List.isEmpty clauses then
                Ok deriveStack
            else
                let clause = List.head clauses
                match unify clause.head goal with
                | Ok bindings ->
                    deriveStack.Push({ clause = clause; bindings = bindings })
                    Ok deriveStack
                | Error error ->
                    Error error


    let query (machine: Machine) (goal: Term) =
        match search machine goal with
        | Ok clauses ->
            let clauseStack = Stack()
            clauseStack.Push(clauses)
            let deriveStack = Stack()
            match derive machine clauseStack deriveStack goal with
            | Ok stack ->
                Ok stack
            | Error error ->
                Error (error)
        | Error error ->
            Error (Search error)
