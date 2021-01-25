namespace Feblr.Logical.Wam

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler
open Feblr.Logical.Unify

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

                match Map.tryFind key machine.indexes with
                | Some indexes ->
                    let indexes =
                        List.append indexes [ List.length database - 1 ]

                    { indexes = Map.add key indexes machine.indexes
                      database = database }
                | None ->
                    let indexes = [ List.length database - 1 ]

                    { indexes = Map.add key indexes machine.indexes
                      database = database }
            | None -> machine

    let load (clauses: Clause list) =
        List.fold Machine.append Machine.empty clauses

    type Goal = Term

    type Target =
        | Term of Term
        | Clause of Clause

    type TreeNode =
        { target: Target
          bindings: Binding list
          children: TreeNode list }

    let rec rewrite (bindings: Binding list) (term: Term) =
        match term with
        | Var _ ->
            match List.tryFind (fun (termA, termB) -> termA = term) bindings with
            | Some (termA, termB) -> termB
            | None -> term
        | CompoundTerm (functor, arguments) ->
            CompoundTerm(rewrite bindings functor, List.map (rewrite bindings) arguments)
        | _ -> term

    let unify (term: Term) (clause: Clause) =
        let result = robinson term clause.head

        match result with
        | Ok bindings -> Some(clause, bindings)
        | Error error -> None

    let search (machine: Machine) (term: Term) =
        let key =
            match term with
            | Atom atom -> Some $"{atom}/0"
            | CompoundTerm (Atom atom, arguments) -> Some $"{atom}/{List.length arguments}"
            | _ -> None

        match key with
        | Some key ->
            match Map.tryFind key machine.indexes with
            | Some indexes ->
                let clauses =
                    List.map (fun index -> List.item index machine.database |> unify term) indexes
                    |> List.filter Option.isSome
                    |> List.map Option.get

                if List.isEmpty clauses then
                    None
                else
                    Some clauses
            | None -> None
        | None -> None

    let rec derive (machine: Machine) (term: Term): TreeNode option =
        match search machine term with
        | Some clauses ->
            let children =
                clauses
                |> List.map
                    (fun (clause, bindings) ->
                        if List.isEmpty clause.body then
                            Some
                                { target = Clause clause
                                  bindings = bindings
                                  children = [] }
                        else
                            let children =
                                List.map (fun term -> rewrite bindings term |> derive machine) clause.body
                                |> List.filter Option.isSome
                                |> List.map Option.get

                            if List.isEmpty children then
                                None
                            else if List.length children <> List.length clause.body then
                                None
                            else
                                Some
                                    { target = Clause clause
                                      bindings = bindings
                                      children = children })
                |> List.filter Option.isSome
                |> List.map Option.get

            if List.isEmpty children then
                None
            else
                let treeNode =
                    { target = Term term
                      bindings = []
                      children = children }

                Some treeNode
        | None -> None

    let query (machine: Machine) (goal: Goal) = derive machine goal
