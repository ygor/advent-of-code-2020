module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module List =
    let span predicate lst = List.takeWhile (predicate) lst, List.skipWhile (predicate) lst
    
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
