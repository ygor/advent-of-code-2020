open System.IO
open Extensions

let numbers =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> List.map int

let say (last: int) (turn: int) (spoken: (int * int) []) =
    let add value =
        spoken.[value] <- turn, fst spoken.[value]
        value, turn + 1, spoken

    let said = spoken.[last]
    if snd said < 0 then add 0 else add (fst said - snd said)

let rec nextTurn (last, turn, spoken) maxTurns =
    if turn = maxTurns then last else nextTurn (say last turn spoken) maxTurns

let readStartingNumbers (numbers: int list) (spoken: (int * int) []) =
    numbers
    |> List.mapi (fun i x -> i, x)
    |> List.fold (fun (spoken': (int * int) []) (i, x) ->
        spoken'.[x] <- (i, -1)
        spoken') spoken

let play n =
    let spoken = readStartingNumbers numbers (Array.create n (-1, -1))
    nextTurn (numbers |> List.last, numbers.Length, spoken) n

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (play 2020)
    printfn "Part 2: %A" (play 30_000_000)
    0
