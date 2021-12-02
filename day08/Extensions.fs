module Extensions

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Result =
    let isOk result =
        match result with
        | Ok _ -> true
        | Error _ -> false
        
module List =
    let replaceAt i value list =
        List.take i list
        @ [ value ]
        @ List.skip (i + 1) list
