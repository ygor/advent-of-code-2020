module Extensions

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray

module List =
    let apply (fList: ('a -> 'b) list) (xList: 'a list) =
        [ for f in fList do
            for x in xList do
                yield f x ]

    let (<*>) = apply

    let (<!>) = List.map

    let splitBy sep list =
        let rec split (acc, group) list' =
            match list' with
            | x :: xs ->
                if x <> sep then split (acc, group @ [ x ]) xs
                else split (acc @ [ group ], []) xs
            | _ -> acc @ [ group ], []

        split ([], []) list |> fst
