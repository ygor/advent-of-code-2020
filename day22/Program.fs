open System.IO
open Extensions

type Deck = int list
type Player = string

let game: Deck * Deck =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.unpack2
    |> Tuple.map2 (fun text ->
        match text |> String.split "\n" with
        | _ :: cards -> cards |> List.map int
        | [] -> failwithf "Invalid input: %s" text)

let score deck =
    deck
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (i, x) -> (i + 1) * x)

let rec play game =
    match game with
    | [], ys -> score ys
    | xs, [] -> score xs
    | x :: xs, y :: ys ->
        if x > y
        then play (xs @ [ x; y ], ys)
        else play (xs, ys @ [ y; x ])

[<EntryPoint>]
let main _ =
    printfn "Part1: %A" (play game)
    0
