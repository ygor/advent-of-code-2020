open System.IO
open System.Numerics
open System.Text.RegularExpressions
open Extensions

type Expression =
    | Leaf of string
    | Node of Expression list

let input =
    File.ReadAllLines("input.txt")
    |> List.ofArray
    |> List.map (fun line ->
        let collection = Regex.Matches(line, "\d+|[+*()]")
        seq { for m in collection do yield m.Value } |> Seq.toList)

let rec parse (stack: Expression list) (input: string list) =
    match input with
    | "(" :: xs -> parse (Leaf "(" :: stack) xs
    | ")" :: xs ->
        let left, right = List.span ((<>) (Leaf "(")) stack
        let stack' = if (not right.IsEmpty) && List.head right = Leaf "(" then List.tail right else right
        parse (Node (List.rev left) :: stack') xs 
    | op :: xs when op = "*" || op = "+" -> parse (Leaf op :: stack) xs
    | x :: xs -> parse (Leaf x :: stack) xs
    | [] -> List.rev stack    
    
let rec solve (expressions: Expression list) =
    match expressions with
    | Leaf lf :: [] -> BigInteger.Parse lf
    | Node lst :: [] -> solve lst
    | left :: op :: right :: tail ->
        let left', right' = solve [left], solve [right]
        let op' = if op = Leaf "*" then (*) else (+)
        solve (Leaf (string (op' left' right')) :: tail)
    | x -> failwithf "Invalid expression: %A" x

let part1 input =
    input |> List.sumBy (parse List.empty >> solve)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (part1 input)
    0
