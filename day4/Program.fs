open System.IO
open System.Text.RegularExpressions
open Extensions

type Passport = Map<string, string>

let isMatch pattern value = Regex.IsMatch(value, pattern)
let inRange number min max = number >= min && number <= max

let isYearInRange (min, max) =
    function
    | Regex "^([0-9]{4})$" [ number ] -> inRange (int number) min max
    | _ -> false

let isValidHeight =
    function
    | Regex "^(\d+)cm$" [ number ] -> inRange (int number) 150 193
    | Regex "^(\d+)in$" [ number ] -> inRange (int number) 59 76
    | _ -> false

let isValid1 (passport: Passport) =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> Seq.fold (fun valid key -> valid && Map.containsKey key passport) true

let isValid2 (passport: Passport) =
    [ ("byr", isYearInRange (1920, 2002))
      ("iyr", isYearInRange (2010, 2020))
      ("eyr", isYearInRange (2020, 2030))
      ("hgt", isValidHeight)
      ("hcl", isMatch "^\#([a-f0-9]{6})$")
      ("ecl", isMatch "^(amb|blu|brn|gry|grn|hzl|oth)$")
      ("pid", isMatch "^([0-9]{9})$") ]
    |> Seq.fold (fun valid (key, validator) ->
        match Map.tryFind key passport with
        | Some value -> valid && validator value
        | None -> false) true

let passports =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> Seq.map (fun value ->
        value
        |> String.split "\n"
        |> Seq.map (String.split " ")
        |> Seq.concat
        |> Seq.map (String.split ":" >> List.unpack2)
        |> Map.ofSeq)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (passports |> Seq.filter isValid1 |> Seq.length)
    printfn "Part 2: %i" (passports |> Seq.filter isValid2 |> Seq.length)
    0