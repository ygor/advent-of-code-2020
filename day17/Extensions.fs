module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
    
module List =
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]

    let (<*>) = apply

module Tuple =
    let fst3 (x, _, _) = x
    let snd3 (_, y, _) = y
    let trd3 (_, _, z) = z

    let fst4 (x, _, _, _) = x
    let snd4 (_, y, _, _) = y
    let trd4 (_, _, z, _) = z    
    let fth4 (_, _, _, w) = w
    
    let flatten3 (x, (y, z)) = x, y, z

    let flatten4 (x, (y, (z, w))) = x, y, z, w
    
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
