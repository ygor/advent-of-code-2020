open System.IO
open Extensions
open Extensions.Tuple

type Point = int list
type Cubes = Set<Point>

let cubes =
    File.ReadAllLines("input.txt")
    |> List.ofArray
    |> List.indexed
    |> List.fold (fun cubes (y, line) ->
        line
        |> List.ofSeq
        |> List.indexed
        |> List.fold (fun (cubes': Cubes) (x, c) ->
            match c with
            | '.' -> cubes'
            | '#' -> cubes'.Add([ x; y; 0 ])
            | x -> failwithf "Invalid initial state: %c" x) cubes) Set.empty

let dxs (cubes: Cubes) =
    let dim = Set.maxElement cubes |> List.length
    List.init dim (fun i -> [ -1; 0; 1 ])
    |> List.cartesian

let neighbours (cubes: Cubes) point =
    dxs cubes
    |> List.fold (fun (neighbours': Cubes) dx ->
        List.zip point dx
        |> List.map sum
        |> neighbours'.Add) Set.empty
    |> Set.filter (fun neighbour -> neighbour <> point && cubes.Contains neighbour)

let cycle (cubes: Cubes) =
    let dim = Set.maxElement cubes |> List.length
    let dxs = List.init dim (fun i -> [ -1; 0; 1 ]) |> List.cartesian

    cubes
    |> Set.fold (fun points point ->
        dxs
        |> List.fold (fun (points': Cubes) dx ->
            List.zip point dx |> List.map sum |> points'.Add) points) Set.empty
    |> Set.fold (fun (cubes': Cubes) point ->
        let num = neighbours cubes point |> Set.count
        if num = 3 then cubes'.Add point
        elif num = 2 && cubes.Contains point then cubes'.Add point
        else cubes') Set.empty

let boot cubes =
    [ 0 .. 5 ]
    |> List.fold (fun cubes' _ -> cycle cubes') cubes
    |> Set.count

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (boot cubes)
    printfn "Part 2: %A" (boot (cubes |> Set.map (fun point -> point @ [0])))
    0
