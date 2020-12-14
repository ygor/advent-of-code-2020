open System
open System.IO
open Extensions

let input =
    File.ReadAllLines("input.txt") |> List.ofSeq

type Emulator =
    { Mask: string
      Memory: Map<int64, int64> }

    static member Create =
        { Mask = ""
          Memory = Map.empty<int64, int64> }

let parseMask1 mask =
    [ Convert.ToInt64(Regex.replace "X" "0" mask, 2), (|||)
      Convert.ToInt64(Regex.replace "X" "1" mask, 2), (&&&) ]

let parseMask2 mask =
    [ Convert.ToInt64(Regex.replace "X" "0" mask, 2), (|||) ]

let applyMask mask value =
    mask
    |> List.fold (fun result (m, op) -> op m result) value

let update1 system address value =
    { system with
          Memory = system.Memory.Add(address, applyMask (parseMask1 system.Mask) value) }

let rec generateAddresses mask input (addresses: char list list) =
    match mask, input with
    | [], [] -> addresses
    | x :: xs, y :: ys ->
        let heads = if y = 'X' then [ '1'; '0' ] else [ x ]
        addresses
        |> List.map (fun address -> heads |> List.map (List.pure' >> List.append address))
        |> List.concat
        |> generateAddresses xs ys
    | x -> failwithf "Mask and input should be of the same length: %A" x

let update2 system address value =
    let masked = applyMask (parseMask2 system.Mask) address
    let maskedLiteral = Convert.ToString(masked, 2).PadLeft(system.Mask.Length, '0')
    let addressLiterals = generateAddresses (maskedLiteral |> List.ofSeq) (system.Mask |> List.ofSeq) [[]]
    
    addressLiterals |> List.fold (fun system' address ->
        let address' = Convert.ToInt64(address |> Array.ofList |> String, 2)
        { system' with
              Memory = system'.Memory.Add(address', value) }) system

let run update =
    input
    |> Seq.fold (fun system line ->
        match line with
        | Regex "mem\[(\d+)\] = (\d+)" [ address; value ] -> update system (int64 address) (int64 value)
        | Regex "mask = (.*)" [ mask ] -> { system with Mask = mask }
        | x -> failwithf "Invalid input: %s" x) Emulator.Create

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (Map.sumBy snd (run update1).Memory)
    printfn "Part 2: %A" (Map.sumBy snd (run update2).Memory)
    0
