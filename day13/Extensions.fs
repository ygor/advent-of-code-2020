module Extensions

open System.Text.RegularExpressions

module Regex =
    let replace (pattern:string) (replacement:string) (input:string) =
        Regex.Replace(input, pattern, replacement)
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray

module Tuple =
    let trd (_, _, c) = c

module List =
    let zipWith f xs ys = List.zip xs ys |> List.map (fun (x,y) -> f x y)
    let zipWith3 f xs ys zs = List.zip (List.zip xs ys) zs |> List.map (fun ((x,y), z) -> f (f x y) z)
    
    let unpack2 list =
        match list with
        | [ a; b ] -> a, b
        | _ -> failwithf "Tried to unpack2 list without exactly 2 elements: %A" list    