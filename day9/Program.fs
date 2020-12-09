open System.IO
open Extensions
open Extensions.List

let numbers =
    File.ReadAllLines("input.txt")
    |> Seq.map int64
    |> List.ofSeq

let part1 (numbers: int64 list) =
    numbers
    |> List.windowed 26
    |> List.find (fun window ->
        window.[0..(Seq.length window - 2)]
        |> List.combinations 2
        |> List.map List.sum
        |> (not << List.contains (List.last window)))
    |> List.last

let part2 target numbers =
    let rec part2' sum start (numbers': int64 list) =
        match sum with
        | value when value < target -> part2' (value + numbers'.[start]) (start + 1) numbers' 
        | value when value > target -> part2' (value - List.head numbers') (start - 1) (List.tail numbers') 
        | _ -> [List.max; List.min] <*> [List.take start numbers'] |> List.sum

    part2' 0L 0 numbers

[<EntryPoint>]
let main _ =
    let number = part1 numbers
    printfn "Part 1: %A" number
    printfn "Part 2: %A" (part2 number numbers)
    0
