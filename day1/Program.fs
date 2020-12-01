open System.IO

let numbers =
    File.ReadAllLines("input.txt")
    |> Seq.toList
    |> List.map int

let rec part1 sum = function
    | [] -> 0
    | x :: xs ->
        if Seq.contains (sum - x) xs then x * (sum - x) else (part1 sum xs)

let rec part2 sum = function
    | [] -> 0
    | x :: xs ->
        match part1 (sum - x) xs with
        | 0 -> part2 sum xs
        | y -> x * y 

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1 2020 numbers}"
    printfn $"Part 2: {part2 2020 numbers}"
    0
