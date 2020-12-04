open System.IO
open Extensions

type Passport = Map<string, string>

let lines = File.ReadAllLines("input.txt")

let inRange min max number = int number >= min && int number <= max

let isValidNumber (min, max) value =
    match value with
    | Regex "^([0-9]{4})$" [ number ] -> inRange min max number
    | _ -> false

let isValidHeight value =
    match value with
    | Regex "^(\d+)cm$" [ number ] -> inRange 150 193 number
    | Regex "^(\d+)in$" [ number ] -> inRange 59 76 number
    | _ -> false

let isValidHairColor value =
    match value with
    | Regex "^\#([a-f0-9]{6})$" [ _ ] -> true
    | _ -> false

let isValidEyeColor value =
    match value with
    | Regex "^(amb|blu|brn|gry|grn|hzl|oth)$" [ _ ] -> true
    | _ -> false

let isValidPid value =
    match value with
    | Regex "^([0-9]{9})$" [ _ ] -> true
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
    [ ("byr", isValidNumber (1920, 2002))
      ("iyr", isValidNumber (2010, 2020))
      ("eyr", isValidNumber (2020, 2030))
      ("hgt", isValidHeight)
      ("hcl", isValidHairColor)
      ("ecl", isValidEyeColor)
      ("pid", isValidPid) ]
    |> Seq.fold (fun valid (key, validator) ->
        match Map.tryFind key passport with
        | Some value -> valid && validator value
        | None -> false) true

let addFields (passport: Passport) (line: string) =
    line.Split(' ')
    |> Seq.fold (fun (passport': Passport) pair ->
        let data = pair.Split(":")
        passport'.Add(data.[0], data.[1])) passport

let passports =
    lines
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