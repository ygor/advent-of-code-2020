open System.IO
open Extensions

let group operation input =
    input
    |> String.split "\n"
    |> Seq.map Set.ofSeq
    |> Seq.reduce operation

let count operation =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> Seq.sumBy (group operation >> Seq.length)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (count Set.union)
    printfn "Part 2: %i" (count Set.intersect)
    0