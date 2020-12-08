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

    if List.contains pointer' stack then Error acc
    elif pointer' < 0 || pointer' >= List.length instructions then Ok acc
    else run pointer' acc' (pointer' :: stack) instructions

let rec patch acc patches instructions =
    match instructions with
    | x :: xs ->
        let patches' =
            match x with
            | Nop n -> (acc @ [ Jmp n ] @ xs) :: patches
            | Jmp n -> (acc @ [ Nop n ] @ xs) :: patches
            | _ -> patches

        patch (acc @ [ x ]) patches' xs
    | [] -> patches

let part2 instructions =
    instructions
    |> patch [] []
    |> List.map (run 0 0 [])
    |> List.filter (fun res ->
        match res with
        | Ok _-> true
        | Error _ -> false)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (run 0 0 [] instructions)
    printfn "Part 2: %A" (part2 instructions)
    0