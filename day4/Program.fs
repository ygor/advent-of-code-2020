open System.IO
open System.Text.RegularExpressions
open Extensions

type Passport = Map<string, string>

let isYearInRange (min, max) = function
    | Regex "^([0-9]{4})$" [ number ] -> Int.inRange (min, max) (int number)
    | _ -> false

let isValidHeight  = function
    | Regex "^(\d+)cm$" [ number ] -> Int.inRange (150, 193) (int number)
    | Regex "^(\d+)in$" [ number ] -> Int.inRange (59, 76) (int number)
    | _ -> false

let isValidHairColor value = Regex.IsMatch(value, "^\#([a-f0-9]{6})$")
let isValidEyeColor value = Regex.IsMatch(value, "^(amb|blu|brn|gry|grn|hzl|oth)$")
let isValidPID value = Regex.IsMatch(value, "^([0-9]{9})$")

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
      ("hcl", isValidHairColor)
      ("ecl", isValidEyeColor)
      ("pid", isValidPID) ]
    |> Seq.fold (fun valid (key, validator) ->
        match Map.tryFind key passport with
        | Some value -> valid && validator value
        | None -> false) true

let addFields (passport: Passport) (line: string) =
    line
    |> String.split " "
    |> Seq.fold (fun (passport': Passport) pair ->
        pair
        |> String.split ":"
        |> List.unpack2
        |> passport'.Add) passport

let passports =
    File.ReadAllLines("input.txt")
    |> Seq.fold (fun (passports: Passport list, passport: Passport) line ->
        if line = ""
        then passport :: passports, Map.empty<string, string>
        else passports, addFields passport line) ([], Map.empty<string, string>)
    |> fun (passports, passport) -> passport :: passports

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (passports |> Seq.filter isValid1 |> Seq.length)
    printfn "Part 2: %i" (passports |> Seq.filter isValid2 |> Seq.length)
    0
