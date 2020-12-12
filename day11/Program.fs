open System.IO
open Extensions

let input =
    File.ReadAllLines("input.txt")
    |> Seq.map (List.ofSeq)
    |> List.ofSeq

let grid =
    let width = input |> List.map (List.length) |> List.max
    Array2D.init width (input.Length) (fun x y -> input.[y].[x])

let directions = List.allPairs [ -1 .. 1 ] [ -1 .. 1 ] |> List.except [ (0, 0) ]

let onGrid grid (x, y) = x >= 0 && x < (Array2D.length1 grid) && y >= 0 && y < (Array2D.length2 grid)

let adjacents (x, y) grid =
    directions
    |> List.map (fun (a, b) -> (x + a, y + b))
    |> List.filter (onGrid grid)
    |> List.map (fun (x, y) -> grid.[x, y])

let print grid =
    [ 0 .. (Array2D.length2 grid - 1) ]
    |> List.map (fun y ->
        grid.[*, y]
        |> Array.map string
        |> Array.reduce (+)
        |> printfn "%s")

let onlyEmptySeats positions = List.forall (fun pos -> pos = 'L' || pos = '.') positions

let atLeastOccupied n positions = (List.filter ((=) '#') positions |> List.length) >= n

let numOccupiedSeats grid =
    grid
    |> Array2D.toSeq
    |> Seq.filter (snd >> (=) '#')
    |> Seq.length

let update value n positions =
    match value with
    | 'L' -> if onlyEmptySeats positions then '#' else 'L'
    | '#' -> if atLeastOccupied n positions then 'L' else '#'
    | x -> x

let rec evolve grid initializer =
    let grid' = Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (initializer grid)
    if grid = grid' then grid else evolve grid' initializer

let initializer1 grid x y = adjacents (x, y) grid |> update grid.[x, y] 4    

let rec seatInSight grid (x, y) (dx, dy) =
    let x', y' = x + dx, y + dy
    if onGrid grid (x', y') then
        if grid.[x', y'] <> '.' then Some grid.[x', y'] else seatInSight grid (x', y') (dx, dy)
    else
        None

let seats grid (x, y) =
    directions
    |> List.map (seatInSight grid (x, y))
    |> List.fold (fun acc -> function
        | Some pos -> pos :: acc
        | None -> acc) []

let initializer2 grid x y = seats grid (x, y) |> update grid.[x, y] 5   

[<EntryPoint>]
let main _ =
    let grid' = evolve grid initializer1
    print grid' |> ignore
    printfn "Part 1: %A" (numOccupiedSeats grid')

    let grid'' = evolve grid initializer2
    print grid'' |> ignore
    printfn "Part 2: %A" (numOccupiedSeats grid'')
    0
