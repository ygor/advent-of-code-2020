open System.IO
open Extensions

let adapters =
    File.ReadAllLines("input.txt")
    |> Seq.map int
    |> List.ofSeq
    |> List.sort

let part1 (adapters: int list) =
    let pairs = adapters |> List.pairwise

    let ones =
        pairs
        |> List.filter (fun (a, b) -> b - a = 1)
        |> List.length

    let threes =
        pairs
        |> List.filter (fun (a, b) -> b - a = 3)
        |> List.length

    ones * threes

let diffs adapters =
    adapters
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)

let rec groups (acc, group) list =
    match list with
    | x :: xs ->
        if x <> 3 then groups (acc, group @ [ x ]) xs
        elif List.isEmpty group then groups (acc, group) xs
        else groups (acc @ [ group ], []) xs
    | _ -> if List.isEmpty group then (acc, []) else (acc @ [ group ], [])

let rec count group =
    match group with
    | x :: y :: xs when (x = 1 && y = 1) -> count (1 :: xs) + count (2 :: xs)
    | x :: y :: xs when (x = 1 && y = 2) -> count (3 :: xs) + count (2 :: xs)
    | x :: y :: xs when (x = 2 && y = 1) -> count (3 :: xs) + count (1 :: xs)
    | x :: xs -> count xs
    | [] -> 1L

let part2 diffs =
    groups ([], []) diffs
    |> fst
    |> List.map count
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    let adapters' = 0 :: adapters @ [ List.max adapters + 3 ]

    printfn "Part 1: %A" (part1 adapters')
    printfn "Part 2: %A" (part2 (diffs adapters'))
    0
