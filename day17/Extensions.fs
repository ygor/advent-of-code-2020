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
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
