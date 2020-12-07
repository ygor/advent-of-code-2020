open System.IO
open Extensions

type Bags = Map<string, (int * string) list>

let parseBag input =
    match input with
    | Regex "(\d+) (.*) bags?" [ number; name ] -> int number, name
    | _ -> failwithf "Invalid input %s" input

let bags =
    File.ReadAllLines("input.txt")
    |> Seq.fold (fun bags line ->
        let outer, contain =
            line |> String.split " contain " |> List.unpack2

        let inners =
            match contain with
            | "no other bags." -> []
            | _ -> String.split ", " contain |> List.map parseBag

        Map.add (outer.Replace(" bags", "")) inners bags) Map.empty<string, (int * string) list>

let containers =
    bags
    |> Map.fold (fun res outer inners ->
        inners
        |> List.fold (fun (res': Map<string, string Set>) (_, inner) ->
            match res'.TryFind inner with
            | Some containers ->
                res'
                |> Map.remove inner
                |> Map.add inner (Set.add outer containers)
            | None -> Map.add inner (Set.ofList [ outer ]) res') res) Map.empty<string, string Set>

let rec part1 id containers =
    match Map.tryFind id containers with
    | Some outers ->
        outers
        |> Set.union
            (outers
             |> Set.map (fun id' -> part1 id' containers)
             |> Set.unionMany)
    | None -> Set.empty

let rec part2 id bags =
    match Map.tryFind id bags with
    | Some inners ->
        inners
        |> List.sumBy (fun (number, id') -> number * (part2 id' bags))
        |> (+) 1
    | None -> 1

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (part1 "shiny gold" containers |> Set.count)
    printfn "Part 2: %i" ((part2 "shiny gold" bags) - 1)
    0
