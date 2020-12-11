module Extensions

module List =
    let countBy predicate list =
        list
        |> List.filter predicate
        |> List.length
        
    let splitBy sep list =
        let rec split (acc, group) list' =
            match list' with
            | x :: xs ->
                if x <> sep then split (acc, group @ [ x ]) xs
                else split (acc @ [ group ], []) xs
            | _ -> acc @ [ group ], []

        split ([], []) list |> fst
