open System.IO
open Extensions

let numbers =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> List.map int

let next (numbers: int list) (lookup: Map<int, int list>) =
    match lookup.[List.head numbers] with
    | x :: y :: _ ->
        let value = x - y
        let previous = if lookup.ContainsKey value then List.take 1 lookup.[value] else []
        
        value :: numbers,
        lookup.Add (value, numbers.Length :: previous)
    | _ :: [] -> (0 :: numbers), lookup.Add(0,  numbers.Length :: List.take 1 lookup.[0])
    | [] -> numbers, lookup

let lookup (numbers: int list) =
    numbers
    |> List.mapi (fun i x -> i, x)
    |> List.fold (fun (lookup': Map<int, int list>) (i, x) ->
        lookup'.Add(x, [ i ])) Map.empty<int, int list>

let rec part1 (numbers, lookup) round =
    if round = 0 then List.head numbers else part1 (next numbers lookup) (round - 1)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (part1 (numbers |> List.rev, (lookup numbers)) (2020 - List.length numbers))
    printfn "Part 2: %A" (part1 (numbers, (lookup numbers)) (1000000 - List.length numbers))
    0
