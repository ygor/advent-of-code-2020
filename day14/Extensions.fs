module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module List =
    let pure' value =
        [ value ]
        
module Regex =
    let replace (pattern:string) (replacement:string) (input:string) =
        Regex.Replace(input, pattern, replacement)

module Map =
    let sumBy (projection: 'a * 'b -> int64) map =
        map
        |> Map.toSeq
        |> Seq.sumBy projection
        
module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray
