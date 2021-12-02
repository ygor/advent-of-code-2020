open System.IO

let lines = File.ReadAllLines("input.txt")

let trees (lines: string seq) (right, down) =
    lines
    |> Seq.mapi (fun i line -> i, line)
    |> Seq.filter (fun (i, _) -> i % down = 0)
    |> Seq.mapi (fun j (_, line) -> if line.[(right * j) % line.Length] = '#' then 1 else 0)
    |> Seq.sum

let traverse slopes =
    slopes |> Seq.map (trees lines) |> Seq.reduce (*)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (traverse [ (3, 1) ])

    printfn
        "Part 2: %d"
        (traverse [ (1, 1)
                    (3, 1)
                    (5, 1)
                    (7, 1)
                    (1, 2) ])
    0
