open System.IO
open Extensions

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let instructions =
    File.ReadAllLines("input.txt")
    |> Seq.fold (fun (instructions: Instruction list) line ->
        let instruction =
            match line with
            | Regex "acc ((\+|\-)\d+)" [ number; _ ] -> Acc(int number)
            | Regex "jmp ((\+|\-)\d+)" [ number; _ ] -> Jmp(int number)
            | Regex "nop ((\+|\-)\d+)" [ number; _ ] -> Nop(int number)
            | _ -> failwithf "Invalid input %s" line

        List.append instructions [ instruction ]) []

let rec run (pointer: int) (acc: int) (stack: int list) (instructions: Instruction list) =
    let pointer', acc' =
        match instructions.[pointer] with
        | Acc n -> (pointer + 1), (acc + n)
        | Jmp n -> (pointer + n), acc
        | Nop _ -> (pointer + 1), acc

    if pointer' = instructions.Length then
        Ok acc
    elif List.contains pointer' stack
         || pointer' < 0
         || pointer' > instructions.Length then
        Error acc
    else
        run pointer' acc' (pointer' :: stack) instructions

let patch (instructions: Instruction list) =
    [ 0 .. (instructions.Length - 1) ]
    |> List.fold (fun acc i ->
        match instructions.[i] with
        | Nop n -> (List.replaceAt i (Jmp n) instructions) :: acc
        | Jmp n -> (List.replaceAt i (Nop n) instructions) :: acc
        | _ -> acc) []

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (run 0 0 [] instructions)

    patch instructions
    |> List.map (run 0 0 [])
    |> List.filter (Result.isOk)
    |> printfn "Part 2: %A"
    0
