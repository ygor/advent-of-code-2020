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
    |> Seq.map
        (fun line -> seq { for m in Regex.Matches(line, "e|se|sw|w|nw|ne") do yield vectors.[m.Value] } |> Seq.toList)

let flip floor tile =
    if Map.containsKey tile floor
    then Map.add tile (if floor.[tile] = Black then White else Black) floor
    else Map.add tile Black floor

let newFloor =
    instructions
    |> Seq.fold (fun floor' instruction ->
        instruction
        |> List.fold (fun cx vector -> (fst cx + fst vector, snd cx + snd vector)) (0.0, 0.0)
        |> flip floor') Map.empty
    
let countBlack floor =
    floor
    |> Map.filter (fun _ color -> color = Black)
    |> Map.count

let part1 = countBlack newFloor

let adjacents tile =
    vectors
    |> Map.toList |> List.map (snd >> fun (x, y) -> (x + fst tile, y + snd tile))

let flipBlacks floor =
    floor
    |> Map.filter (fun tile color ->
        color = Black &&
        adjacents tile
        |> List.filter (fun cx -> Map.containsKey cx floor && floor.[cx] = Black)
        |> List.length
        |> (fun count -> count = 0 || count > 2))
    |> Map.map (fun _ _ -> White) 
 
let flipWhites floor =
    let keys = Map.keys floor
    let maxX, minX = Seq.maxBy fst keys |> fst, Seq.minBy fst keys |> fst
    let maxY, minY = Seq.maxBy snd keys |> snd, Seq.minBy snd keys |> snd
    
    List.allPairs [minX - 1.0 .. maxX + 1.0] [minY - 1.0 .. maxY + 1.0]
    |> List.filter (fun tile ->
        (not (Map.containsKey tile floor) || floor.[tile] = White) &&
        adjacents tile
        |> List.filter (fun cx -> Map.containsKey cx floor && floor.[cx] = Black)
        |> List.length
        |> (fun count -> count = 2))
    |> List.map (fun cx -> (cx, Black))
    |> Map.ofList

let dayFlip floor =
    floor
    |> Map.merge (flipBlacks floor)
    |> Map.merge (flipWhites floor)

let part2 =
    newFloor
    |> Func.repeat 100 dayFlip
    |> countBlack
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: %i{part2}"
    0