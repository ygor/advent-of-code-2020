open System
open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")
    
let rotate (x, y) degrees =
    let radians = Math.PI * (degrees / 180.0)
    (x * (Math.Cos radians) - y * (Math.Sin radians) |> Math.Round,
     x * (Math.Sin radians) + y * (Math.Cos radians) |> Math.Round)

let manhattan (x: float, y: float) = int (Math.Abs x + Math.Abs y)

let part1 =
    input
    |> Seq.fold (fun ((x, y), dir) instruction ->
        match instruction with
        | Regex "N(\d+)" [ units ] -> (x, y + float units), dir
        | Regex "S(\d+)" [ units ] -> (x, y - float units), dir
        | Regex "E(\d+)" [ units ] -> (x + float units, y), dir
        | Regex "W(\d+)" [ units ] -> (x - float units, y), dir
        | Regex "F(\d+)" [ units ] -> (x + float units * fst dir, y + float units * snd dir), dir
        | Regex "L(\d+)" [ degrees ] -> (x, y), rotate dir (float degrees)
        | Regex "R(\d+)" [ degrees ] -> (x, y), rotate dir (-1.0 * float degrees)        
        | x -> failwithf "Invalid instruction %s" x) ((0.0, 0.0), (1.0, 0.0))
    |> (fst >> manhattan)

let part2 =
    input
    |> Seq.fold (fun ((x, y), (wx, wy)) instruction ->
        match instruction with
        | Regex "N(\d+)" [ units ] -> (x, y), (wx, wy + float units)
        | Regex "S(\d+)" [ units ] -> (x, y), (wx, wy - float units)
        | Regex "E(\d+)" [ units ] -> (x, y), (wx + float units, wy)
        | Regex "W(\d+)" [ units ] -> (x, y), (wx - float units, wy)
        | Regex "F(\d+)" [ units ] -> (x + float units * wx, y + float units * wy), (wx, wy)
        | Regex "L(\d+)" [ degrees ] -> (x, y), rotate (wx, wy) (float degrees)
        | Regex "R(\d+)" [ degrees ] -> (x, y), rotate (wx, wy) (-1.0 * float degrees)
        | x -> failwithf "Invalid instruction %s" x) ((0.0, 0.0), (10.0, 1.0))
    |> ( fst >> manhattan)

[<EntryPoint>]
let main _ =
    printfn "Part1: %i" part1
    printfn "Part2: %i" part2    
    0