open System.IO

let numbers =
    File.ReadAllLines("input.txt")
    |> Seq.toList
    |> List.map int

let rec part1 sum = function
    | [] -> 0
    | x :: xs ->
        if Seq.contains (sum - x) xs then x * (sum - x) else (part1 sum xs)

let rec part2 = function
    | [] -> 0
    | x :: xs ->
        match part1 (2020 - x) xs with
        | 0 -> part2 xs
        | y -> x * y 

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (part1 2020 numbers)
    printfn "Part 2: %i" (part2 numbers)
    0
