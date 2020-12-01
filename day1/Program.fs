open System.IO

let numbers =
    File.ReadAllLines("input.txt")
    |> Seq.toList
    |> List.map int

let rec pairs = function
    | [] -> []
    | x :: xs ->
        xs
        |> List.map (fun b -> x, b)
        |> List.append (pairs xs)

let rec triplets = function
    | [] -> []
    | x :: xs ->
        pairs xs
        |> List.map (fun (a, b) -> x, a, b)
        |> List.append (triplets xs)

[<EntryPoint>]
let main argv =
    let (a, b) =
        numbers
        |> pairs
        |> Seq.find (fun (a, b) -> a + b = 2020)

    printfn "Part 1: sum: %i" (a * b)

    let (a, b, c) =
        numbers
        |> triplets
        |> Seq.find (fun (a, b, c) -> a + b + c = 2020)

    printfn "Part 2: sum: %i" (a * b * c)
    0
