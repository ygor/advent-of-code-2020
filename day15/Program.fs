open System.IO
open Extensions

let numbers =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> List.map int

let say (last: int) (lastIndex: int) (spoken: int []) =
    let next = if spoken.[last] >= 0 && spoken.[last] <> lastIndex then lastIndex - spoken.[last] else 0
    spoken.[last] <- lastIndex
    next, lastIndex + 1, spoken

let rec nextTurn (last, lastIndex, spoken) maxTurns =
    if lastIndex = (maxTurns - 1) then last else nextTurn (say last lastIndex spoken) maxTurns

let readStartingNumbers (numbers: int list) (spoken: int []) =
    numbers
    |> List.mapi (fun i x -> i, x)
    |> List.fold (fun (spoken': int []) (i, x) ->
        spoken'.[x] <- i
        spoken') spoken

let play n =
    let spoken = readStartingNumbers numbers (Array.create n -1)
    nextTurn (numbers |> List.last, numbers.Length - 1, spoken) n

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (play 2020)
    printfn "Part 2: %A" (play 30_000_000)
    0
