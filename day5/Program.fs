open System
open System.IO

let seat pass =
    pass
    |> Seq.fold (fun acc char ->
        acc
        + (if Seq.contains char "BR" then "1" else "0")) ""
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