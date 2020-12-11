module Extensions

module Array2D =
    let toSeq array =
        seq {
            for r in 0 .. (Array2D.length1 array - 1) do
                for c in 0 .. (Array2D.length2 array - 1) do
                    yield array.[r, c]
        }

module List =
    let countBy predicate list =
        list |> List.filter predicate |> List.length

    let splitBy sep list =
        let rec split (acc, group) list' =
            match list' with
            | x :: xs -> if x <> sep then split (acc, group @ [ x ]) xs else split (acc @ [ group ], []) xs
            | _ -> acc @ [ group ], []

        split ([], []) list |> fst
