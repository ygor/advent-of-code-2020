open System.IO
open Extensions

type Policy = int * int * char
type Password = string

let parseLine (line: string): Policy * Password =
    match line with
    | Regex "(\d+?)-(\d+?) ([a-z]+): (\w+)" [ min; max; letter; password ] -> (int min, int max, char letter), password
    | x -> failwithf "Invalid instruction: %A" x

let list =
    File.ReadAllLines("input.txt")
    |> Seq.map parseLine

let rentalValidator ((min, max, letter): Policy, password: Password) =
    let count =
        password
        |> Seq.filter (fun c -> c = letter)
        |> Seq.length

    count >= min && count <= max

let tobogganValidator ((pos1, pos2, letter): Policy, password: Password) =
    (password.[pos1 - 1] = letter)
    <> (password.[pos2 - 1] = letter)

let validPasswords validator =
    list |> Seq.filter validator |> Seq.length

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (validPasswords rentalValidator)
    printfn "Part 2: %i" (validPasswords tobogganValidator)
    0
