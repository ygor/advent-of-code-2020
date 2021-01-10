open System
open System.IO
open System.Numerics
open Extensions

type Rows = char list list
type TileId = bigint * int
type Adjacents = Map<TileId, Map<TileId, int list>>

let tiles =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.map (fun text ->
        let lines = String.split "\n" text
        let id = BigInteger.Parse ((List.head lines).Substring(5, 4))
        (id, 0), List.tail lines |> List.map List.ofSeq)

let seaMonster = File.ReadLines("sea_monster.txt") |> List.ofSeq |> List.map Seq.toList 

let transformations =
    [ 0 .. 3 ]
    |> List.collect (fun i ->
        let rotate' = Fun.repeat i Matrix.rotate
        [ rotate'; rotate' >> List.rev ])

let variants =
    tiles
    |> List.collect (fun ((id, _), rows) ->
        transformations |> List.mapi (fun i f -> (id, i), f rows))
    |> Map.ofList

let borders (rows: Rows) =
    [ List.head rows
      rows |> List.map List.last
      List.last rows
      rows |> List.map List.head ]
    |> List.indexed

let adjacents: Adjacents =
    variants
    |> Map.map (fun (id, _) rows ->
        let vBorders = borders rows
        variants
        |> Map.filter (fun (id', _) _ -> id <> id')
        |> Map.map (fun _ rows' ->
            borders rows'
            |> List.filter (fun (i, border) -> snd vBorders.[(i + 2) % 4] = border)
            |> List.map fst)
        |> Map.filter (fun _ edges -> edges.IsEmpty |> not))

let corners: Adjacents = adjacents |> Map.filter (fun _ neighbours -> neighbours.Count = 2)

let topLeft =
    corners
    |> Map.findKey (fun _ neighbours ->
        Map.toList neighbours
        |> List.map (snd >> Set.ofList)
        |> List.reduce Set.union
        |> (=) ([ 0; 3 ] |> Set.ofList))

let neighbour tileId edge =
    adjacents.[tileId] |> Map.findKey (fun _ edges -> List.contains ((edge + 2) % 4) edges)

let crop (rows: Rows) = rows.[1..(rows.Length - 2)] |> List.map (fun row -> row.[1..(row.Length - 2)])

let gridSize = float tiles.Length |> Math.Sqrt |> int

let assemble (grid: Map<int * int, TileId>) =
    let croppedTileSize = variants.[topLeft].Length - 2
    let length = gridSize * croppedTileSize

    Array2D.init length length (fun x y ->
        let x', y' = x / croppedTileSize, y / croppedTileSize
        let rows = crop variants.[grid.[(x', y')]]
        rows.[y % croppedTileSize].[x % croppedTileSize])

let image =
    [ 0 .. gridSize - 1 ]
    |> Seq.fold (fun grid x ->
        [ 0 .. gridSize - 1 ]
        |> Seq.fold (fun (grid': Map<int * int, TileId>) y ->
            let neighbour =
                match (x, y) with
                | (0, 0) -> topLeft
                | (0, y) -> neighbour grid'.[0, y - 1] 2
                | (x, y) -> neighbour grid'.[x - 1, y] 1
                
            grid'.Add((x, y), neighbour)) grid) Map.empty<int * int, TileId>
    |> assemble

let imageVariants =
    let rows = image |> Array2D.toList
    transformations |> List.map (fun f -> f rows) |> List.map Array2D.fromList

let isSeaMonsterAt x y (image: char [,]) =
    let l1, l2 = Array2D.length1 image, Array2D.length2 image

    [ 0 .. seaMonster.Length - 1 ]
    |> Seq.fold (fun found' y' ->
        [ 0 .. seaMonster.Head.Length - 1 ]
        |> Seq.fold (fun found x' ->
            match seaMonster.[y'].[x'] with
            | '#' ->
                let x'', y'' = x + x', y + y'
                found && x'' < l1 && y'' < l2 && image.[x'', y''] = '#'
            | _ -> found) found') true

let countSeaMonsters (image: char [,]) =
    [ 0 .. (Array2D.length1 image) - 1 ]
    |> Seq.fold (fun count' x ->
        [ 0 .. (Array2D.length2 image) - 1 ]
        |> Seq.fold (fun count y -> if isSeaMonsterAt x y image then count + 1 else count) count') 0

let countHashes (rows: Rows) = rows |> List.concat |> List.filter ((=) '#') |> List.length

let part1 =
    corners
    |> Map.toList
    |> List.map (fst >> fst)
    |> List.unique
    |> Seq.reduce (*)

let part2 =
    let numSeaMonsterHashes = countHashes seaMonster
    imageVariants
    |> List.map (fun rows -> countHashes (rows |> Array2D.toList) - (countSeaMonsters rows * numSeaMonsterHashes)) 
    |> List.min

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    printfn "Part 2: %A" part2
    0