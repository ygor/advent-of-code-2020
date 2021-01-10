open System.IO
open Extensions

type Ingredient = string
type Allergen = string
type Food = Ingredient Set * Allergen Set

let foods: Food list =
    File.ReadAllLines("input.txt")
    |> Array.toList
    |> List.map (fun line ->
        match line with
        | Regex "(.*) \(contains (.*)\)" [ ingredients; allergens ] ->
            String.split " " ingredients |> Set.ofList, String.split ", " allergens |> Set.ofList
        | x -> failwithf "Invalid input: %s" x) 
        
let allergens =
    foods
    |> List.map snd
    |> List.reduce Set.union
    |> Set.toList
    |> List.map (fun allergen ->
        allergen, foods |> List.filter (fun food -> snd food |> Set.contains allergen) |> List.map fst)
    |> Map.ofList

let ingredientsWithoutAllergens =
    let commons, without =
        allergens
        |> Map.map (fun _ foods ->
            let common = foods |> List.reduce Set.intersect
            common,
            foods |> List.map (fun ingredients -> Set.difference ingredients common) |> List.reduce Set.union)
        |> Map.fold (fun (commons, without) _ (commons', without') ->
            Set.union commons commons', Set.union without without') (Set.empty, Set.empty)
    Set.difference without commons
let part1 =
    foods
    |> List.map (fun food -> fst food |> Set.intersect ingredientsWithoutAllergens |> Set.count)
    |> List.reduce (+)
        
[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    0
