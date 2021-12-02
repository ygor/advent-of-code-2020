open System.IO
open System.Text.RegularExpressions
open Extensions

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
    |> Seq.map (fun line -> Regex.Matches(line, "e|se|sw|w|nw|ne") |> Seq.map (fun m -> vectors.[m.Value]))

let flip floor tile =
    if Map.containsKey tile floor
    then Map.add tile (if floor.[tile] = Black then White else Black) floor
    else Map.add tile Black floor

let newFloor =
    instructions
    |> Seq.fold (fun floor' instruction ->
        instruction
        |> Seq.fold (fun cx vector -> (fst cx + fst vector, snd cx + snd vector)) (0.0, 0.0)
        |> flip floor') Map.empty
    
let countBlack floor =
    floor
    |> Map.filter (fun _ color -> color = Black)
    |> Map.count

let adjacents tile =
    vectors
    |> Map.values
    |> Seq.map (fun (x, y) -> (x + fst tile, y + snd tile))

let flipBlacks floor =
    floor
    |> Map.filter (fun tile color ->
        color = Black &&
        adjacents tile
        |> Seq.lengthBy (fun adj -> Map.containsKey adj floor && floor.[adj] = Black)
        |> (fun length -> length = 0 || length > 2))
    |> Map.map (fun _ _ -> White) 
 
let flipWhites floor =
    let keys = Map.keys floor
    let maxX, minX = Seq.maxBy fst keys |> fst, Seq.minBy fst keys |> fst
    let maxY, minY = Seq.maxBy snd keys |> snd, Seq.minBy snd keys |> snd
    
    List.allPairs [minX - 1.0 .. 0.5 .. maxX + 1.0] [minY - 1.0 .. 0.5 .. maxY + 1.0]
    |> List.filter (fun tile ->
        ((Map.containsKey tile floor |> not) || floor.[tile] = White) &&
        adjacents tile
        |> Seq.lengthBy (fun adj -> Map.containsKey adj floor && floor.[adj] = Black)
        |> (fun length -> length = 2))
    |> List.map (fun tile -> (tile, Black))
    |> Map.ofList

let dayFlip floor =
    floor
    |> Map.merge (flipWhites floor)
    |> Map.merge (flipBlacks floor)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{countBlack newFloor}"
    printfn $"Part 2: %i{newFloor |> Func.repeat 100 dayFlip |> countBlack}"
    0