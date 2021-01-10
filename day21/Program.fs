open System.IO
open Extensions

type Ingredient = string
type Allergen = string
type Food = string Set * string Set

let foods: Food list =
    File.ReadAllLines("input.txt")
    |> Array.toList
    |> List.map (fun line ->
        match line with
        | Regex "(.*) \(contains (.*)\)" [ ingredients; allergens ] ->
            String.split " " ingredients |> Set.ofList, String.split ", " allergens |> Set.ofList
        | x -> failwithf "Invalid input: %s" x)

let (ingredients: Ingredient Set), (allergens: Allergen Set) =
    foods |> List.unzip |> Tuple.map2 Set.unionMany

let possible: Ingredient Set =
    allergens
    |> Set.map (fun allergen ->
        foods
        |> List.filter (fun (_, allergens) -> allergens.Contains allergen)
        |> List.map fst
        |> Set.intersectMany)
    |> Set.unionMany

let inert = ingredients - possible

let rec identify identified : Set<Ingredient * Allergen> =
    let identified' =
        (allergens - Set.map snd identified)
        |> Set.fold (fun identified' allergen ->
            let options =
                foods
                |> List.filter (fun (_, allergens) -> allergens.Contains allergen)
                |> List.map fst
                |> List.fold Set.intersect ingredients
                |> (fun ingredients -> ingredients - inert - Set.map fst identified')

            if options.Count = 1 then identified'.Add(options.MaximumElement, allergen) else identified') identified

    if identified'.Count = possible.Count then identified' else identify identified'

let part1 =
    foods
    |> List.map (fun (ingredients, _) -> Set.intersect inert ingredients |> Set.count)
    |> List.reduce (+)

let part2 =
    identify Set.empty
    |> Set.toList
    |> List.sortBy snd
    |> List.map fst
    |> List.reduce (fun a b -> a + "," + b)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    printfn "Part 2: %A" part2
    0
