open System
open System.IO
open Extensions

let adjacents (x, y) area =
    [ (x - 1, y + 1)
      (x - 1, y - 1)
      (x - 1, y)
      (x + 1, y - 1)
      (x + 1, y)
      (x + 1, y + 1)
      (x, y - 1)
      (x, y + 1) ]
    |> List.filter (fun (x, y) ->
        x
        >= 0
        && x < Array2D.length1 area
        && y >= 0
        && y < Array2D.length2 area)

let input =
    File.ReadAllLines("input.txt")
    |> Seq.map (List.ofSeq)
    |> List.ofSeq

let area =
    let width =
        input |> List.map (List.length) |> List.max

    Array2D.init width (input.Length) (fun x y -> input.[y].[x])

let allSeatsEmpty positions =
    positions
    |> List.forall (fun pos -> pos = 'L' || pos = '.')

let atLeastFourOccupied positions =
    positions
    |> List.filter ((=) '#')
    |> List.length
    >= 4

let rule1 area x y =
    let adjacents' =
        adjacents (x, y) area
        |> List.map (fun (x, y) -> area.[x, y])

    match area.[x, y] with
    | 'L' -> if allSeatsEmpty adjacents' then '#' else 'L'
    | '#' -> if atLeastFourOccupied adjacents' then 'L' else '#'
    | x -> x

let next area rule =
    Array2D.init (Array2D.length1 area) (Array2D.length2 area) (rule area)

let rec run area rule =
    let area' = next area rule
    if area = area' then area else run area' rule

let occupied area =
    area
    |> Array2D.toSeq
    |> Seq.filter (fun value -> value = '#')
    |> Seq.length

let print area =
    [ 0 .. (Array2D.length2 area - 1) ]
    |> List.map (fun y ->
        area.[*, y]
        |> Array.map string
        |> Array.reduce (+)
        |> printfn "%s")

let length (x, y) = Math.Sqrt(x * x + y * y)

[<EntryPoint>]
let main _ =
    let area' = run area rule1
    print area' |> ignore
    printfn "Part 1: %A" (occupied area')
    0
