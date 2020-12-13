open System
open System.IO
open Extensions

let input =
    File.ReadAllLines("input.txt") |> List.ofSeq

let timestamp = input |> List.head

let ids =
    input
    |> List.last
    |> String.split ","
    |> List.map (Regex.replace "x" "0" >> int >> bigint)

let part1 =
    let id, wait =
        ids
        |> List.filter ((<) 0I)
        |> List.map (fun id -> id, id - ((bigint (int timestamp)) % id))
        |> List.sortBy snd
        |> List.head

    id * wait

let extendedEuclid (x: bigint) (y: bigint) =
    let x0, x1, y0, y1 = 1I, 0I, 0I, 1I
    
    let rec inner (x, y, x0, x1, y0, y1) =
        if y > 0I then
            let q, x', y' = x / y, y, x % y
            let x0', x1' = x1, x0 - q * x1
            let y0', y1' = y1, y0 - q * y1
            inner (x', y', x0', x1', y0', y1')
        else
            x, x0, y0
    
    inner (x, y, x0, x1, y0, y1) 
    
let chineseRemainder (bs: bigint list) (ns: bigint list) =
    let N = ns |> List.fold (*) 1I
    let Ns = ns |> List.map (fun n -> N / n)
    let ms = List.zipWith extendedEuclid ns Ns |> List.map Tuple.trd 
    let As = List.zipWith3 (*) bs ms Ns
    List.sum As % N

let part2 =
    let system =
        ids
        |> List.mapi (fun i id -> i, id)
        |> List.filter (snd >> ((<) (bigint 0)))
        |> List.map (fun (i, id) -> (id - (bigint i % id)) % id, id)

    chineseRemainder (system |> List.map fst) (system |> List.map snd)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" part1
    printfn "Part 2: %A" part2
    0
