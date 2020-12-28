open System.IO
open System.Text.RegularExpressions
open Extensions

type Rule =
    | Value of string
    | Ids of int list
    | Or of Rule * Rule

let input =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.unpack2

let parseIds input = input |> String.split " " |> List.map int |> Ids

let parseRule input =
    match input with
    | Regex "(\d+): (.*) \| (.*)" [ id; ids1; ids2 ] -> int id, Or(parseIds ids1, parseIds ids2)
    | Regex "(\d+): \"(.*)\"" [ id; value ] -> int id, Value value
    | Regex "(\d+): (.*)" [ id; ids ] -> int id, parseIds ids
    | x -> failwithf "Invalid rule: %s" x

let rules =
    fst input
    |> String.split "\n"
    |> List.map parseRule
    |> Map.ofList

let build (rules: Map<int, Rule>) maxLength =
    let rec loop length = function
        | Value value -> value
        | Ids ids ->
            if length > maxLength then ""
            else
                ids
                |> List.map (fun id -> loop (length + ids.Length) rules.[id])
                |> List.reduce (+)
        | Or (r1, r2) -> "(" + loop length r1 + "|" + loop length r2 + ")"

    "^" + loop 0 rules.[0] + "$"
    
let rules2 =
    rules
    |> Map.add 8 (Or (Ids [ 42 ], Ids [ 42; 8 ]))
    |> Map.add 11 (Or (Ids [ 42; 31 ], Ids [ 42; 11; 31 ]))

let validate input (rules: Map<int, Rule>) =
    let messages = input |> String.split "\n"
    let maxLength = messages |> List.map String.length |> List.max
    let regex = build rules maxLength
    
    messages
    |> List.filter (fun s -> Regex.IsMatch(s, regex))
    |> List.length

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (validate (snd input) rules)
    printfn "Part 2: %A" (validate (snd input) rules2)
    0
