open System.IO
open Extensions

let cups =
    File.ReadAllText("input.txt")
    |> Seq.map (string >> int)
    |> Seq.toList

let pick3 current cups =
    let length = List.length cups
    let index = (List.findIndex ((=) current) cups + 1) % length
    CyclicList.takeManyAt index 3 cups, CyclicList.removeManyAt index 3 cups

let rec selectDestination current hand cups =
    let min, max = List.min cups, List.max cups

    let label = if current - 1 < min then max else current - 1
    if List.contains label cups then label
    else selectDestination label hand cups

let place3 destination hand cups =
    let index = List.findIndex ((=) destination) cups
    List.insertManyAt (index + 1) hand cups

let neighbour current cups =
    let length = List.length cups
    let index = (List.findIndex ((=) current) cups + 1) % length
    cups.[index]

let move (current: int) (cups: int list) =
    let hand, cups' = pick3 current cups
    let destination = selectDestination current hand cups'
    let cups'' = place3 destination hand cups'
    neighbour current cups'', cups''

let part1 =
    [ 0 .. 99 ]
    |> List.fold (fun (current, cups) _ -> move current cups) (List.head cups, cups)
    |> snd

[<EntryPoint>]
let main _ =
    printfn $"Part1: %A{part1}"
    0
