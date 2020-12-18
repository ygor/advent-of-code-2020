open System.IO
open Extensions
open Extensions.Tuple

type Point = int * int * int

type Tube =
    | Active
    | InActive

type Grid = Map<int * int * int, Tube>

let flatten ((x, y), z) = x, y, z

let minBy f list =
    list |> List.minBy (fst >> f) |> (fst >> f)

let maxBy f list =
    list |> List.maxBy (fst >> f) |> (fst >> f)

let neighbours (x, y, z) =
    List.allPairs (List.allPairs [ -1; 0; 1 ] [ -1; 0; 1 ]) [ -1; 0; 1 ]
    |> List.map flatten
    |> List.filter (fun (dx, dy, dz) -> (dx, dy, dz) <> (0, 0, 0))
    |> List.map (fun (dx, dy, dz) -> x + dx, y + dy, z + dz)

let boundaries (grid: Grid) =
    let values = grid |> Map.toList

    let xMax, xMin =
        values |> maxBy fst3, values |> minBy fst3

    let yMax, yMin =
        values |> maxBy snd3, values |> minBy snd3

    let zMax, zMin =
        values |> maxBy trd3, values |> minBy trd3

    List.allPairs (List.allPairs [ xMin - 1 .. xMax + 1 ] [ yMin - 1 .. yMax + 1 ]) [ zMin - 1 .. zMax + 1 ]
    |> List.map flatten
    |> List.except (grid |> Map.toList |> List.map (fst))

let tubes (grid: Grid) points =
    points
    |> List.map (fun (x, y, z) -> (x, y, z), (if grid.ContainsKey(x, y, z) then grid.[x, y, z] else InActive))

let rec extend grid =
    let boundaries = boundaries grid |> tubes grid

    let numActives =
        boundaries
        |> List.filter (snd >> (=) Active)
        |> List.length

    let grid' =
        (grid |> Map.toList) @ boundaries |> Map.ofList

    if (numActives > 0) then extend grid' else grid'

let numActiveNeighbours grid (x, y, z) =
    neighbours (x, y, z)
    |> tubes grid
    |> List.filter (snd >> (=) Active)
    |> List.length

let cycle (grid: Grid) =
    let grid' = extend grid

    grid'
    |> Map.toList
    |> Seq.fold (fun (result: Grid) ((x, y, z), state) ->
        let num = numActiveNeighbours grid' (x, y, z)

        let value =
            match state with
            | Active -> if num = 2 || num = 3 then Active else InActive
            | InActive -> if num = 3 then Active else InActive

        result.Add((x, y, z), value)) Map.empty<int * int * int, Tube>

let print (grid: Grid) z =
    let values = grid |> Map.toList

    let xMax, xMin =
        values |> maxBy fst3, values |> minBy fst3

    let yMax, yMin =
        values |> maxBy snd3, values |> minBy snd3

    [ yMin .. yMax ]
    |> Seq.iter (fun y ->
        [ xMin .. xMax ]
        |> List.map (fun x ->
            match grid.[x, y, z] with
            | Active -> '#'
            | InActive -> '.')
        |> List.toArray
        |> System.String
        |> printfn "%s")

let grid =
    let data =
        File.ReadAllLines("input.txt") |> List.ofArray

    data
    |> List.indexed
    |> List.fold (fun grid (y, line) ->
        line
        |> List.ofSeq
        |> List.indexed
        |> List.fold (fun (grid': Grid) (x, c) ->
            let value =
                match c with
                | '.' -> InActive
                | '#' -> Active
                | x -> failwithf "Invalid initial state: %c" x

            grid'.Add((x, y, 0), value)) grid) Map.empty<int * int * int, Tube>

let part1 (grid: Grid) =
    let grid'' =
        [ 0 .. 5 ]
        |> List.fold (fun grid' _ -> cycle grid') grid

    print grid'' 0

    grid''
    |> Map.toSeq
    |> Seq.filter (fun (_, value) -> value = Active)
    |> Seq.length

[<EntryPoint>]
let main _ =
    print grid 0
    printfn "%A" (part1 grid)
    0
