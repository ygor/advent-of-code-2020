open System.IO
open Extensions

let cups =
    File.ReadAllText("input.txt")
    |> Seq.map (string >> int)
    |> Seq.toList

let pick current (ring: Map<int, int>) =
    let hand = [0 .. 1] |> List.fold (fun hand _ -> hand @ [ring.[List.last hand]]) [ring.[current]]
    hand, ring |> Map.removeMany hand |> Map.add current ring.[List.last hand] 

let rec destination current hand ring (min, max) =
    let label = if current - 1 < min then max else current - 1
    if List.contains label hand then destination label hand ring (min, max) else label

let place dest hand ring =
    let ring'', prev =
        hand |> List.fold (fun (ring', prev) cup -> Map.add prev cup ring', cup) (ring |> Map.remove dest, dest)
    ring'' |> Map.add prev ring.[dest]

let move current ring (min, max) =
    let hand, ring' = pick current ring
    let destination = destination current hand ring' (min, max)
    let ring'' = place destination hand ring'
    ring''.[current], ring''

let play moves cups =
    let min, max = List.min cups, List.max cups
    
    [ 0 .. moves - 1 ]
    |> List.fold (fun (current, ring) _ -> move current ring (min, max))
        (List.head cups, List.zip cups (List.tail cups @ [List.head cups])  |> Map.ofList)
    |> snd

let score (ring: Map<int, int>) =
    bigint ring.[1] * bigint ring.[ring.[1]]    

[<EntryPoint>]
let main _ =
    printfn $"Part1: %A{play 100 cups}"
    printfn $"Part2: %A{play 10000000 (cups @ [10 .. 1000000]) |> score}"
    0
