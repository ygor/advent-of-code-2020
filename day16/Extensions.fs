module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
    
module List =
    let unpack3 list =
        match list with
        | [ a; b; c ] -> a, b, c
        | _ -> failwithf "Tried to unpack2 list without exactly 3 elements: %A" list
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
