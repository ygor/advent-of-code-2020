module Extensions

open System.Text.RegularExpressions
module Regex =
    let replace (pattern:string) (replacement:string) (input:string) =
        Regex.Replace(input, pattern, replacement)