open System.IO
open Extensions

let cups =
    File.ReadAllText("input.txt")
    |> Seq.map (string >> int)
    |> Seq.toList

let rec destination current hand ring (min, max) =
    let label = if current - 1 < min then max else current - 1
    if List.contains label hand then destination label hand ring (min, max) else label
    
let move current (ring: Map<int, int>) (min, max) =
    let hand = [0 .. 1] |> List.fold (fun hand _ -> hand @ [ring.[List.last hand]]) [ring.[current]]
    let dest = destination current hand ring (min, max)

    ring.[List.last hand], ring
       |> Map.removeMany [current; List.last hand; dest]
       |> Map.addMany [(current, ring.[List.last hand]); (dest, List.head hand); (List.last hand, ring.[dest])]

let play moves cups =
    let min, max = List.min cups, List.max cups
    
    [ 0 .. moves - 1 ]
    |> List.fold (fun (current, ring) _ -> move current ring (min, max))
        (List.head cups, List.zip cups (List.tail cups @ [List.head cups]) |> Map.ofList)
    |> snd

let score (ring: Map<int, int>) =
    bigint ring.[1] * bigint ring.[ring.[1]]    

[<EntryPoint>]
let main _ =
    printfn $"Part1: %A{play 100 cups}"
    printfn $"Part2: %A{play 10000000 (cups @ [10 .. 1000000]) |> score}"
    0
