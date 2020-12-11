open System.IO
open Extensions
open Extensions.List

let adapters =
    File.ReadAllLines("input.txt")
    |> Seq.map int
    |> List.ofSeq
    |> List.sort

let diffs adapters =
    adapters
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)

let part1 (adapters: int list) =
    let diffs' = diffs adapters
    [List.length] <*> ([ List.filter ((=) 1); List.filter ((=) 3) ] <*> [diffs']) |> List.reduce (*)   

let rec count group =
    match group with
    | x :: y :: xs when (x = 1 && y = 1) -> count (1 :: xs) + count (2 :: xs)
    | x :: y :: xs when (x = 1 && y = 2) -> count (3 :: xs) + count (2 :: xs)
    | x :: y :: xs when (x = 2 && y = 1) -> count (3 :: xs) + count (1 :: xs)
    | x :: xs -> count xs
    | [] -> 1L

let part2 (diffs: int list) =
    splitBy 3 diffs
    |> List.filter (List.isEmpty >> not)
    |> List.map count
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    let adapters' = 0 :: adapters @ [ List.max adapters + 3 ]

    printfn "Part 1: %A" (part1 adapters')
    printfn "Part 2: %A" (part2 (diffs adapters'))
    0
