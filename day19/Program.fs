open System.IO
open Extensions

type Rule =
    | Value of string
    | Ids of int list
    | Or of Rule * Rule

let input =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.unpack2

let parseIds input =
    input |> String.split " " |> List.map int |> Ids

let parseRule input =
    match input with
    | Regex "(\d+): (.*) \| (.*)" [ id; ids1; ids2 ] -> int id, Or(parseIds ids1, parseIds ids2)
    | Regex "(\d+): \"(.*)\"" [ id; value ] -> int id, Value value
    | Regex "(\d+): (.*)" [ id; ids ] -> int id, parseIds ids
    | x -> failwithf "Invalid rule: %s" x

let rules =
    fst input
    |> String.split "\n"
    |> List.map (parseRule)
    |> Map.ofList

let flatten (rules: Map<int, Rule>) =
    let rec loop rule cont =
        match rule with
        | Value value -> cont [ value ]
        | Ids (id :: ids) ->
            loop rules.[id] (fun acc ->
                loop (Ids ids) (fun acc' ->
                    List.allPairs acc acc'
                    |> List.map (fun (xs, ys) -> xs + ys)
                    |> cont))
        | Ids [] -> cont [ "" ]
        | Or (r1, r2) -> loop r1 (fun r1acc -> loop r2 (fun r2acc -> cont (r1acc @ r2acc)))

    loop rules.[0] id

let rules2 =
    rules
    |> Map.add 8 (Or(Ids [ 42 ], Ids [ 42; 8 ]))
    |> Map.add 11 (Or(Ids [ 42; 31 ], Ids [ 42; 11; 31 ]))

let run rules =
    let rules' = flatten rules

    snd input
    |> String.split "\n"
    |> List.filter (fun s -> List.contains s rules')
    |> List.length

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (run rules)
//    printfn "Part 2: %A" (run rules2)
    0
