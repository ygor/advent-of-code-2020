open System
open System.IO
open Extensions

let seat pass =
    pass
    |> Regex.replace "B|R" "1"
    |> Regex.replace "F|L" "0"
    |> (fun value -> Convert.ToInt32(value, 2))

let seats =
    File.ReadAllLines("input.txt")
    |> Seq.map seat
    |> Seq.sort

let emptySeat seats =
    Seq.except seats [ Seq.min seats .. Seq.max seats ]
    |> Seq.head

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (seats |> Seq.max)
    printfn "Part 2: %i" (seats |> emptySeat)
    0
