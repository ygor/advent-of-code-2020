open System.IO
open System.Text.RegularExpressions

type Op =
    | Plus
    | Times
    
type Expression =
    | Empty
    | Value of int
    | Op of Op * Expression * Expression
    
let input =
    File.ReadAllLines("input.txt")
    |> List.ofArray
    |> List.map (fun line ->
        let collection = Regex.Matches(line, "\d+|[+*()]")
        seq { for m in collection do yield m.Value } |> Seq.toList) 

let rec parse input =
    
    
    match input with
    | x :: xs ->
        match x with
        | Regex "\d+" [ value ]-> parse xs (Value (int value))
        | Regex "+|*" [ op ] -> Op (op, lhs, parse xs Empty) 
        | Regex "\(" [ openBracket ] ->
        | Regex "\)" [ closeBracket ] ->
        | z -> failwithf "Unknown expression: %s" z
    | [] -> 
            
    
        
