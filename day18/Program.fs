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
        let stack' = if not right.IsEmpty && List.head right = Leaf "(" then List.tail right else right
        parse (Node (List.rev left) :: stack') xs
    | x :: xs -> parse (Leaf x :: stack) xs
    | [] -> List.rev stack    
    
let rec solver1 (expressions: Expression list) =
    match expressions with
    | Leaf lf :: [] -> BigInteger.Parse lf
    | Node lst :: [] -> solver1 lst
    | left :: op :: right :: tail ->
        let left', right' = solver1 [left], solver1 [right]
        let op' = if op = Leaf "*" then (*) else (+)
        solver1 (Leaf (string (op' left' right')) :: tail)
    | x -> failwithf "Invalid expression: %A" x

let rec solver2 (expressions: Expression list) =
    match expressions with
    | Leaf lf :: [] -> BigInteger.Parse lf
    | Node lst :: [] -> solver2 lst
    | left :: op :: right :: [] ->
        let left', right' = solver2 [left], solver2 [right]
        let op' = if op = Leaf "*" then (*) else (+)
        solver2 (Leaf (string (op' left' right')) :: [])
    | left :: op1 :: right :: op2 :: tail ->
        if op1 = Leaf "*" && op2 = Leaf "+" then solver2 (left :: op1 :: [Leaf ( string (solver2 (right :: op2 :: tail)) )])
        else solver2 (Leaf (string (solver2 (left :: op1 :: [right]))) :: op2 :: tail)
    | x -> failwithf "Invalid expression: %A" x

let solve input solver =
    input |> List.sumBy (parse List.empty >> solver)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (solve input solver1)
    printfn "Part 2: %A" (solve input solver2)
    0