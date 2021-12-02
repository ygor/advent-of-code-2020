open System.IO
open Extensions

type Bags = Map<string, (int * string) list>

let parseBag input =
    match input with
    | Regex "(\d+) (.*) bags?" [ number; name ] -> int number, name
    | _ -> failwithf "Invalid input %s" input

let bags =
    File.ReadAllLines("input.txt")
    |> Seq.fold (fun (bags: Bags) line ->
        match line with
        | Regex "no other" [] -> bags
        | Regex "(.*) bags contain (.*)\." [ outer; contain ] ->
            bags.Add(outer.Replace(" bags", ""), String.split ", " contain |> List.map parseBag)
        | _ -> failwithf "Invalid input %s" line) Map.empty<string, (int * string) list>

let rec part1 id bags =
    bags
    |> Map.toSeq
    |> Seq.filter (snd >> List.map snd >> List.contains id)
    |> Seq.map (fun (inner, _) -> Set.add inner (part1 inner bags))
    |> Seq.fold Set.union Set.empty

let rec part2 id bags =
    match Map.tryFind id bags with
    | Some inners ->
        inners
        |> List.sumBy (fun (number, id') -> number * (part2 id' bags))
        |> (+) 1
    | None -> 1

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (part1 "shiny gold" bags |> Set.count)
    printfn "Part 2: %i" ((part2 "shiny gold" bags) - 1)
    0
