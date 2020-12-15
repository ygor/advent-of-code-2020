open System.IO
open Extensions

let input =
    File.ReadAllLines("input.txt") |> List.ofSeq

type Mask = { One: int64; Zero: int64; Float: int64 }
type Emulator = { Mask: Mask; Memory: Map<int64, int64> }

let parseMask input =
    let parse value =
        input
        |> List.ofSeq
        |> List.fold (fun mask c -> mask <<< 1 ||| (if c = value then 1L else 0L)) 0L

    { One = parse '1';  Zero = parse '0'; Float = parse 'X' }

let update1 system address value =
    { system with
          Memory = system.Memory.Add(address, (value ||| system.Mask.One) &&& (~~~system.Mask.Zero)) }

let rec generateAddresses (address: int64) (floating: int64) =
    let floatingBits =
        [ 0 .. 35 ]
        |> List.filter (fun i -> (floating >>> i) &&& 1L = 1L)

    [ 0 .. floatingBits.Length ]
    |> List.map (fun j -> floatingBits |> List.comb j)
    |> List.concat
    |> List.map (List.fold (fun address' i -> address' ||| (1L <<< i)) address)

let update2 system address value =
    generateAddresses (address &&& system.Mask.Zero ||| system.Mask.One) (system.Mask.Float)
    |> List.fold (fun system' address' ->
        { system' with
              Memory = system'.Memory.Add(address', value) }) system

let run update =
    input
    |> Seq.fold (fun system line ->
        match line with
        | Regex "mem\[(\d+)\] = (\d+)" [ address; value ] -> update system (int64 address) (int64 value)
        | Regex "mask = (.*)" [ mask ] -> { system with Mask = parseMask mask }
        | x -> failwithf "Invalid input: %s" x)
            { Mask = { One = 0L; Zero = 0L; Float = 0L };  Memory = Map.empty<int64, int64> }

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (Map.sumBy snd (run update1).Memory)
    printfn "Part 2: %A" (Map.sumBy snd (run update2).Memory)
    0
