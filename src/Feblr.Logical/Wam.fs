namespace Feblr.Logical.Wam

open Feblr.Logical.Compiler.Grammar
open Feblr.Logical.Compiler.Compiler

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

    let query (machine: Machine) (clause: Clause) =
        match clause.head with
        | Atom atom ->
            let key = $"{atom}/0"

            match Map.tryFind key machine.index with
            | Some index ->
                let clause = Seq.item index machine.database
                Some clause
            | None -> None
        | CompoundTerm (Atom atom, arguments) ->
            let key = $"{atom}/{List.length arguments}"

            match Map.tryFind key machine.index with
            | Some index ->
                let clause = Seq.item index machine.database
                Some clause
            | None -> None
        | _ -> None
