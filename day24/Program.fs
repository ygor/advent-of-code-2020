open System.IO
open System.Text.RegularExpressions

type Color = | Black | White

let vectors = Map.ofList [
    ("e", (1.0, 0.0))
    ("se", (0.5, -1.0))
    ("sw", (-0.5, -1.0))
    ("w", (-1.0, 0.0))
    ("nw", (-0.5, 1.0))
    ("ne", (0.5, 1.0))]

let instructions =
    File.ReadAllLines("input.txt")
    |> Seq.map
        (fun line -> seq { for m in Regex.Matches(line, "e|se|sw|w|nw|ne") do yield vectors.[m.Value] } |> Seq.toList)

let flip floor tile =
    if Map.containsKey tile floor
    then Map.add tile (if floor.[tile] = Black then White else Black) floor
    else Map.add tile Black floor

let adjacents tile =
    vectors
    |> Map.toList |> List.map (snd >> fun (x, y) -> (x + fst tile, y + snd tile))

let part1 =
    instructions
    |> Seq.fold (fun floor instruction ->
        instruction
        |> List.fold (fun cx vector -> (fst cx + fst vector, snd cx + snd vector)) (0.0, 0.0)
        |> flip floor) Map.empty
    |> Map.filter (fun _ color -> color = Black)
    |> Map.count

[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    0
