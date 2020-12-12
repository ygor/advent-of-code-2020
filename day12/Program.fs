open System
open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")
    
let rotate (x, y) degrees =
    let radians = Math.PI * (degrees / 180.0)
    (x * (Math.Cos radians) - y * (Math.Sin radians) |> Math.Round,
     x * (Math.Sin radians) + y * (Math.Cos radians) |> Math.Round)

let move1 =
    input
    |> Seq.fold (fun (x, y, dir) instruction ->
        match instruction with
        | Regex "N(\d+)" [ units ] -> (x, y + int units, dir)
        | Regex "S(\d+)" [ units ] -> (x, y - int units, dir)
        | Regex "E(\d+)" [ units ] -> (x + int units, y, dir)
        | Regex "W(\d+)" [ units ] -> (x - int units, y, dir)
        | Regex "L(\d+)" [ degrees ] -> (x, y, rotate dir (double degrees))
        | Regex "R(\d+)" [ degrees ] -> (x, y, rotate dir (-1.0 * double degrees))
        | Regex "F(\d+)" [ units ] -> (x + int units * int (fst dir), y + int units * int (snd dir), dir)
        | x -> failwithf "Invalid instruction %s" x) (0, 0, (1.0, 0.0))

let move2 =
    input
    |> Seq.fold (fun (x, y, (wx, wy)) instruction ->
        match instruction with
        | Regex "N(\d+)" [ units ] -> (x, y, (wx, wy + float units))
        | Regex "S(\d+)" [ units ] -> (x, y, (wx, wy - float units))
        | Regex "E(\d+)" [ units ] -> (x, y, (wx + float units, wy))
        | Regex "W(\d+)" [ units ] -> (x, y, (wx - float units, wy))
        | Regex "F(\d+)" [ units ] -> (x + float units * wx, y + float units * wy, (wx, wy))
        | Regex "L(\d+)" [ degrees ] -> (x, y, rotate (wx, wy) (float degrees))
        | Regex "R(\d+)" [ degrees ] -> (x, y, rotate (wx, wy) (-1.0 * float degrees))
        | x -> failwithf "Invalid instruction %s" x) (0.0, 0.0, (10.0, 1.0))

[<EntryPoint>]
let main _ =
    let (x1, y1, _) = move1
    printfn "Part1: %i" (Math.Abs x1 + Math.Abs y1)
    
    let (x2, y2, _) = move2
    printfn "Part2: %i" (Math.Abs x2 + Math.Abs y2 |> int)    
    0