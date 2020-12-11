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

let adjacents (x, y) area =
    let width, height =
        Array2D.length1 area, Array2D.length2 area

    directions
    |> List.map (fun (a, b) -> (x + a, y + b))
    |> List.filter (fun (a, b) -> a >= 0 && a < width && b >= 0 && b < height)

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

let occupied area =
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

let length (x, y) =
    Math.Sqrt(float x ** 2.0 + float y ** 2.0)

let toVector zero coordinate =
    (fst coordinate - fst zero, snd coordinate - snd zero)

let direction (x, y) =
    directions
    |> List.tryFind (fun (a, b) -> Math.Atan2(float x, float y) = Math.Atan2(float a, float b))

let seats area zero =
    area
    |> Array2D.toSeq
    |> Seq.fold (fun map ((x, y), value) ->
        if (x, y) = zero || value = '.' then
            map
        else
            match direction (toVector zero (x, y)) with
            | None -> map
            | Some dir ->
                let positions =
                    if Map.containsKey dir map then map.[dir] else []

                Map.add dir (((x, y), value) :: positions) map) Map.empty<int * int, ((int * int) * char) list>
    |> Map.map (fun _ list -> List.sortBy (fun (pos, _) -> toVector zero pos |> length) list)

let rule2 area x y =
    let firstSeats =
        seats area (x, y)
        |> Map.toList
        |> List.map (fun (_, list) -> List.head list |> snd)

    match area.[x, y] with
    | 'L' -> if allSeatsEmpty firstSeats then '#' else 'L'
    | '#' -> if atLeastOccupied 5 firstSeats then 'L' else '#'
    | x -> x

[<EntryPoint>]
let main _ =
    let area' = run area rule1
    print area' |> ignore
    printfn "Part 1: %A" (occupied area')

    let area'' = run area rule2
    print area'' |> ignore
    printfn "Part 2: %A" (occupied area'')
    0
