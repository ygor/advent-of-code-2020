open System.IO
open Extensions

type Food = string Set * string Set

let foods: Food list =
    File.ReadAllLines("input.txt")
    |> Array.toList
    |> List.map (fun line ->
        match line with
        | Regex "(.*) \(contains (.*)\)" [ ingredients; allergens ] ->
            String.split " " ingredients |> Set.ofList, String.split ", " allergens |> Set.ofList
        | x -> failwithf "Invalid input: %s" x)

let ingredients, allergens =
    foods
    |> List.unzip
    |> Tuple.map2 Set.unionMany

let possible =
    allergens
    |> Set.map (fun allergen ->
        foods
        |> List.filter (fun (_, allergens) -> allergens.Contains allergen)
        |> List.map fst
        |> Set.intersectMany)
    |> Set.unionMany

let without = ingredients - possible

let part1 =
    foods
    |> List.map (fun (ingredients, _) -> Set.intersect without ingredients |> Set.count)
    |> List.reduce (+)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    0
