module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
    
module Tuple =
    let map2 f (l, r) = f l, f r

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
