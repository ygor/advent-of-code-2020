open System.IO

let keys =
    File.ReadAllLines("input.txt")
    |> Seq.map (int >> bigint)
    |> Seq.take 2

let transform (subject: bigint) =
    Seq.unfold (fun (n, state) ->
        let value = (state * subject) % (bigint 20201227)
        Some ((n, value), (n + 1, value))) (1, bigint 1)

let part1 keys =
    transform (bigint 7)
    |> Seq.zip (Seq.last keys |> transform)
    |> Seq.find (fun (_, (_, k)) -> k = Seq.head keys)
    |> fst

[<EntryPoint>]
let main _ =
    printfn $"Part 1 %A{part1 keys}"
    0