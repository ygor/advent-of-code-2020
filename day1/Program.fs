open System.IO

let numbers =
    File.ReadAllLines(@"input.txt")
    |> Seq.toList
    |> List.map int

let rec pairs lst =
    match lst with
    | [] -> []
    | h :: t -> List.map (fun elem -> (h, elem)) t @ pairs t

let rec triplets lst =
    match lst with
    | [] -> []
    | h :: t ->
        pairs t
        |> List.map (fun (a, b) -> h, a, b)
        |> List.append (triplets t)

[<EntryPoint>]
let main argv =
    let (a, b) =
        numbers
        |> pairs
        |> Seq.filter (fun (a, b) -> a + b = 2020)
        |> Seq.head

    printfn "Part 1: sum: %i" (a * b)

    let (a, b, c) =
        numbers
        |> triplets
        |> Seq.filter (fun (a, b, c) -> a + b + c = 2020)
        |> Seq.head

    printfn "Part 2: sum: %i" (a * b * c)
    0
