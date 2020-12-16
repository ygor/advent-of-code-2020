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

    let unpack2 list =
        match list with
        | [ a; b ] -> a, b
        | _ -> failwithf "Tried to unpack2 list without exactly 2 elements: %A" list

    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]   
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
