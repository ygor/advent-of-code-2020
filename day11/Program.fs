open System.IO
open Extensions

type Seat =
    | Empty
    | Occupied

type Position =
    | Floor
    | Seat of Seat

type Area = Position [,]

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

    Array2D.init width (input.Length) (fun x y ->
        match input.[y].[x] with
        | '.' -> Floor
        | 'L' -> Seat Empty
        | '#' -> Seat Occupied
        | x -> failwithf "Invalid input %c" x)

let allSeatsEmpty positions =
    positions
    |> List.forall (fun pos -> pos = Seat Empty || pos = Floor)

let atLeastFourOccupied positions =
    positions
    |> List.filter (fun pos -> pos = Seat Occupied)
    |> List.length
    |> (<) 3

let next (area: Area) =
    Array2D.init (Array2D.length1 area) (Array2D.length2 area) (fun x y ->
        let adjacents' =
            adjacents (x, y) area
            |> List.map (fun (x, y) -> area.[x, y])

        match area.[x, y] with
        | Seat Empty -> if allSeatsEmpty adjacents' then Seat Occupied else Seat Empty
        | Seat Occupied -> if atLeastFourOccupied adjacents' then Seat Empty else Seat Occupied
        | Floor -> Floor)

let rec run (area: Area) =
    let area' = next area
    if area = area' then area else run area'

let occupied (area: Area) =
    seq {
        for r in 0 .. (Array2D.length1 area - 1) do
            for c in 0 .. (Array2D.length2 area - 1) do
                yield area.[r, c]        
    }
    |> Seq.filter (fun value -> value = Seat Occupied)
    |> Seq.length
    
let print (area: Area) =
    [ 0 .. (Array2D.length2 area - 1) ]
    |> List.map (fun y ->
        area.[*, y]
        |> Array.map (fun value ->
            match value with
            | Seat Empty -> "L"
            | Seat Occupied -> "#"
            | Floor -> ".")
        |> Array.reduce (+)
        |> printfn "%s")

[<EntryPoint>]
let main _ =
    let area' = run area
    print area' |> ignore
    printfn "Part 1: %A" (occupied area')
    0
