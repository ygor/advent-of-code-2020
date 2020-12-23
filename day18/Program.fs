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
        seq {
            for m in collection do
                yield m.Value
        }
        |> Seq.toList)

let rec parse (stack: Expression list) (input: string list) =
    match input with
    | "(" :: xs -> parse (Leaf "(" :: stack) xs
    | ")" :: xs ->
        let left, right = List.span ((<>) (Leaf "(")) stack
        let stack' =
            if not right.IsEmpty && List.head right = Leaf "("
            then List.tail right
            else right
        parse (Node(List.rev left) :: stack') xs
    | x :: xs -> parse (Leaf x :: stack) xs
    | [] -> List.rev stack

let rec solver (precedence: Map<string, int>) (expressions: Expression list) =
    match expressions with
    | Leaf lf :: [] -> BigInteger.Parse lf
    | Node lst :: [] -> solver precedence lst
    | left :: op :: right :: [] ->
        let left', right' = solver precedence [ left ], solver precedence [ right ]
        let op' = if op = Leaf "*" then (*) else (+)
        solver precedence [ Leaf <| string (op' left' right') ]
    | left :: (Leaf op1) :: right :: (Leaf op2) :: tail ->
        if precedence.[op2] > precedence.[op1] then
            let sub = solver precedence (right :: Leaf op2 :: tail)
            solver precedence [ left; Leaf op1; Leaf (string sub) ]
        else
            let sub = solver precedence [left; Leaf op1; right ]
            solver precedence (Leaf (string sub) :: Leaf op2 :: tail)
    | x -> failwithf "Invalid expression list: %A" x

let solve input precedence =
    input |> List.sumBy (parse List.empty >> (solver precedence))

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (solve input (["*",1;"+",1] |> Map.ofList))
    printfn "Part 2: %A" (solve input (["*",1;"+",2] |> Map.ofList))
    0