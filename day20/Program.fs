open System.IO
open System.Numerics
open Extensions

type Tile = char list list

let tiles =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.map (fun text ->
        let lines = String.split "\n" text
        (List.head lines).Substring(5, 4) |> BigInteger.Parse, List.tail lines |> List.map (List.ofSeq))

let flipY (tile: Tile) = tile |> List.rev

let rec transpose =
    function
    | ((x :: xs) :: ys) as matrix ->
        List.map List.head matrix
        :: transpose (List.map List.tail matrix)
    | _ -> []

let rotate (tile: Tile) = tile |> transpose |> flipY

let group =
    [ 0 .. 3 ]
    |> List.collect (fun i ->
        let rotate' = Fun.repeat i rotate
        [ rotate'; rotate' >> flipY ])

let print (tile: Tile) =
    tile
    |> List.iter (fun line ->
        line
        |> Array.ofList
        |> System.String
        |> printfn "%s")
    printfn ""

let variants =
    tiles
    |> List.collect (fun tile ->
        group
        |> List.map (fun f -> fst tile, (f << snd) tile))

let borders (tile: Tile) =
    [ List.head tile
      tile |> List.map List.last
      List.last tile
      tile |> List.map List.head ]

let adjacents =
    variants
    |> List.map (fun variant ->
        let vBorders = borders (snd variant)
        fst variant,
        variants
        |> List.filter (fun tile -> fst tile <> fst variant)
        |> List.map (fun tile ->
            fst tile,
            borders (snd tile)
            |> List.indexed
            |> List.filter (fun (i, border) -> vBorders.[(i + 2) % 4] = border)
            |> List.map snd)
        |> List.filter (fun (_, borders) -> (not << List.isEmpty) borders))

let part1 =
    adjacents
    |> List.filter (fun (_, tiles) -> List.length tiles = 2)
    |> List.map fst
    |> List.unique
    |> Seq.reduce (*)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    0
