open System.IO
open Extensions

type Player =
    | Player1
    | Player2

let game =
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

let rec combat game =
    match game with
    | xs, [] -> score xs
    | [], ys -> score ys
    | x :: xs, y :: ys ->
        if x > y
        then combat (xs @ [ x; y ], ys)
        else combat (xs, ys @ [ y; x ])

let recursiveCombat game =
    let rec play game' rounds =
        match game' with
        | _, [] -> Player1, fst game'
        | [], _ -> Player2, snd game'
        | x :: xs, y :: ys ->
            if Set.contains (fst game') (fst rounds) || Set.contains (snd game') (snd rounds)
            then Player1, fst game'
            else
                let rounds' = (fst rounds).Add (fst game'), (snd rounds).Add (snd game')
                let winner = 
                    if xs.Length >= x && ys.Length >= y
                    then play (List.take x xs, List.take y ys) (Set.empty, Set.empty) |> fst
                    elif x > y then Player1 
                    else Player2 
                
                match winner with
                | Player1 -> play (xs @ [ x; y ], ys) rounds'
                | Player2 -> play (xs, ys @ [ y; x ]) rounds'
    
    play game (Set.empty, Set.empty) |> snd |> score

[<EntryPoint>]
let main _ =
    printfn "Part1: %A" (combat game)
    printfn "Part2: %A" (recursiveCombat game)
    0
