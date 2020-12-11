open System
open System.IO
open Extensions

let input =
    File.ReadAllLines("input.txt")
    |> Seq.map (List.ofSeq)
    |> List.ofSeq

let area =
    let width =
        input |> List.map (List.length) |> List.max

    Array2D.init width (input.Length) (fun x y -> input.[y].[x])

let directions =
    List.allPairs [ -1 .. 1 ] [ -1 .. 1 ]
    |> List.except [ (0, 0) ]

let isValidCoordinate area (x, y) =
    x >= 0 && x < (Array2D.length1 area) && y >= 0 && y < (Array2D.length2 area)

let adjacents (x, y) area =
    directions
    |> List.map (fun (a, b) -> (x + a, y + b))
    |> List.filter (isValidCoordinate area)

let print area =
    [ 0 .. (Array2D.length2 area - 1) ]
    |> List.map (fun y ->
        area.[*, y]
        |> Array.map string
        |> Array.reduce (+)
        |> printfn "%s")

let allSeatsEmpty positions =
    positions
    |> List.forall (fun pos -> pos = 'L' || pos = '.')

let atLeastOccupied n positions =
    positions
    |> List.filter ((=) '#')
    |> List.length
    |> (<) (n - 1)

let next area rule =
    Array2D.init (Array2D.length1 area) (Array2D.length2 area) (rule area)

let rec run area rule =
    let area' = next area rule
    if area = area' then area else run area' rule

let numOccupiedSeats area =
    area
    |> Array2D.toSeq
    |> Seq.filter (snd >> (=) '#')
    |> Seq.length

//--- Part 1

let rule1 area x y =
    let positions =
        adjacents (x, y) area
        |> List.map (fun (x, y) -> area.[x, y])

    match area.[x, y] with
    | 'L' -> if allSeatsEmpty positions then '#' else 'L'
    | '#' -> if atLeastOccupied 4 positions then 'L' else '#'
    | x -> x

//--- Part 2

let rec firstVisibleSeat area (x, y) (dx, dy) =
    let x', y' = x + dx, y + dy
    if isValidCoordinate area (x', y') then
        if area.[x', y'] <> '.' then Some (x', y') else firstVisibleSeat area (x', y') (dx, dy)
    else
        None    

let seats area (x, y) =
    directions
    |> List.map (firstVisibleSeat area (x, y))
    |> List.fold (fun acc value ->
        match value with
        | Some (x, y) -> area.[x,y] :: acc
        | None -> acc) []

let rule2 area x y =
    let seats = seats area (x, y)   

    match area.[x, y] with
    | 'L' -> if allSeatsEmpty seats then '#' else 'L'
    | '#' -> if atLeastOccupied 5 seats then 'L' else '#'
    | x -> x

[<EntryPoint>]
let main _ =
    let area' = run area rule1
    print area' |> ignore
    printfn "Part 1: %A" (numOccupiedSeats area')

    let area'' = run area rule2
    print area'' |> ignore
    printfn "Part 2: %A" (numOccupiedSeats area'')
    0
