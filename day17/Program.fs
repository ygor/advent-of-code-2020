open System.IO
open Extensions
open Extensions.Tuple
open Microsoft.VisualBasic.FileIO

type Point3 = int * int * int
type Point4 = int * int * int * int

type Tube =
    | Active
    | InActive

type Grid<'a when 'a : comparison> = Map<'a, Tube>

let minBy f list =
    list |> List.minBy (fst >> f) |> (fst >> f)

let maxBy f list =
    list |> List.maxBy (fst >> f) |> (fst >> f)

let neighbours3 (x: int, y: int, z: int) =
    [ -1; 0; 1 ]
    |> List.allPairs [ -1; 0; 1 ] 
    |> List.allPairs [ -1; 0; 1 ]
    |> List.map flatten3
    |> List.filter (fun (dx, dy, dz) -> (dx, dy, dz) <> (0, 0, 0))
    |> List.map (fun (dx, dy, dz) -> x + dx, y + dy, z + dz)

let neighbours4 (x: int, y: int, z: int, w: int) =
    [ -1; 0; 1 ]
    |> List.allPairs [ -1; 0; 1 ] 
    |> List.allPairs [ -1; 0; 1 ]
    |> List.allPairs [ -1; 0; 1 ]
    |> List.map flatten4
    |> List.filter (fun (dx, dy, dz, dw) -> (dx, dy, dz, dw) <> (0, 0, 0, 0))
    |> List.map (fun (dx, dy, dz, dw) -> x + dx, y + dy, z + dz, w + dw)

let boundaries3 (grid: Grid<Point3>) =
    let values = grid |> Map.toList

    let xMax, xMin =
        values |> maxBy fst3, values |> minBy fst3

    let yMax, yMin =
        values |> maxBy snd3, values |> minBy snd3

    let zMax, zMin =
        values |> maxBy trd3, values |> minBy trd3

    [ zMin - 1 .. zMax + 1 ]
    |> List.allPairs [ yMin - 1 .. yMax + 1 ]
    |> List.allPairs [ xMin - 1 .. xMax + 1 ]
    |> List.map flatten3
    |> List.except (grid |> Map.toList |> List.map (fst))
    
let boundaries4 (grid: Grid<Point4>) =
    let values = grid |> Map.toList

    let xMax, xMin =
        values |> maxBy fst4, values |> minBy fst4

    let yMax, yMin =
        values |> maxBy snd4, values |> minBy snd4

    let zMax, zMin =
        values |> maxBy trd4, values |> minBy trd4

    let wMax, wMin =
        values |> maxBy fth4, values |> minBy fth4
        
    [ wMin - 1 .. wMax + 1 ]
    |> List.allPairs [ zMin - 1 .. zMax + 1 ]
    |> List.allPairs [ yMin - 1 .. yMax + 1 ]
    |> List.allPairs [ xMin - 1 .. xMax + 1 ]
    |> List.map flatten4
    |> List.except (grid |> Map.toList |> List.map (fst))
    

let tubes<'a when 'a : comparison> (grid: Grid<'a>) points =
    points
    |> List.map (fun point -> point, (if grid.ContainsKey(point) then grid.[point] else InActive))

let rec extend3 grid =
    let boundaries = boundaries3 grid |> tubes grid

    let numActives =
        boundaries
        |> List.filter (snd >> (=) Active)
        |> List.length

    let grid' =
        (grid |> Map.toList) @ boundaries |> Map.ofList

    if (numActives > 0) then extend3 grid' else grid'

let rec extend4 grid =
    let boundaries = boundaries4 grid |> tubes grid

    let numActives =
        boundaries
        |> List.filter (snd >> (=) Active)
        |> List.length

    let grid' =
        (grid |> Map.toList) @ boundaries |> Map.ofList

    if (numActives > 0) then extend4 grid' else grid'

let numActiveNeighbours3 grid point =
    neighbours3 point
    |> tubes grid
    |> List.filter (snd >> (=) Active)
    |> List.length

let numActiveNeighbours4 grid point =
    neighbours4 point
    |> tubes grid
    |> List.filter (snd >> (=) Active)
    |> List.length

let cycle3 (grid: Grid<Point3>) =
    let grid' = extend3 grid

    grid'
    |> Map.toList
    |> Seq.fold (fun (result: Grid<Point3>) (point, state) ->
        let num = numActiveNeighbours3 grid' point

        let value =
            match state with
            | Active -> if num = 2 || num = 3 then Active else InActive
            | InActive -> if num = 3 then Active else InActive

        result.Add(point, value)) Map.empty<Point3, Tube>

let cycle4 (grid: Grid<Point4>) =
    let grid' = extend4 grid

    grid'
    |> Map.toList
    |> Seq.fold (fun (result: Grid<Point4>) (point, state) ->
        let num = numActiveNeighbours4 grid' point

        let value =
            match state with
            | Active -> if num = 2 || num = 3 then Active else InActive
            | InActive -> if num = 3 then Active else InActive

        result.Add(point, value)) Map.empty<Point4, Tube>


let print3 (grid: Grid<Point3>) z =
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

let print4 (grid: Grid<Point4>) z w =
    let values = grid |> Map.toList

    let xMax, xMin =
        values |> maxBy fst4, values |> minBy fst4

    let yMax, yMin =
        values |> maxBy snd4, values |> minBy snd4

    [ yMin .. yMax ]
    |> Seq.iter (fun y ->
        [ xMin .. xMax ]
        |> List.map (fun x ->
            match grid.[x, y, z, w] with
            | Active -> '#'
            | InActive -> '.')
        |> List.toArray
        |> System.String
        |> printfn "%s")

let grid3 =
    let data =
        File.ReadAllLines("input.txt") |> List.ofArray

    data
    |> List.indexed
    |> List.fold (fun grid (y, line) ->
        line
        |> List.ofSeq
        |> List.indexed
        |> List.fold (fun (grid': Grid<Point3>) (x, c) ->
            let value =
                match c with
                | '.' -> InActive
                | '#' -> Active
                | x -> failwithf "Invalid initial state: %c" x

            grid'.Add((x, y, 0), value)) grid) Map.empty<Point3, Tube>

let grid4 =
    let data =
        File.ReadAllLines("input.txt") |> List.ofArray

    data
    |> List.indexed
    |> List.fold (fun grid (y, line) ->
        line
        |> List.ofSeq
        |> List.indexed
        |> List.fold (fun (grid': Grid<Point4>) (x, c) ->
            let value =
                match c with
                | '.' -> InActive
                | '#' -> Active
                | x -> failwithf "Invalid initial state: %c" x

            grid'.Add((x, y, 0, 0), value)) grid) Map.empty<Point4, Tube>


let boot<'a when 'a : comparison> (grid: Grid<'a>) cycle =
    let grid'' =
        [ 0 .. 5 ]
        |> List.fold (fun grid' _ -> cycle grid') grid

    grid''
    |> Map.toSeq
    |> Seq.filter (fun (_, value) -> value = Active)
    |> Seq.length

[<EntryPoint>]
let main _ =
    print3 grid3 0
    printfn "Part 1: %A" (boot grid3 cycle3)
    printfn "Part 2: %A" (boot grid4 cycle4)
    0
