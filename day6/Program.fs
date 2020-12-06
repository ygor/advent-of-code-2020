open System.IO
open System.Text.RegularExpressions
open Extensions

let anyoneYes input =
    Regex.Replace(input, "\n", "")
    |> Set.ofSeq
    |> Seq.length    

let allYes input =
    input
    |> String.split "\n"
    |> Seq.map Set.ofSeq
    |> Seq.reduce Set.intersect
    |> Seq.length

let count policy =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> Seq.sumBy policy

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (count anyoneYes)
    printfn "Part 2: %i" (count allYes)
    0