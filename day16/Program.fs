open System.IO
open Extensions

type Ticket = int list
type Range = int * int
type Field = string * Range list

type Notes = { Fields: Field list; YourTicket: Ticket; NearbyTickets: Ticket list }

let parseFields input =
    String.split "\n" input
    |> List.map (fun line ->
        match line with
        | Regex "(.*): (\d+)\-(\d+) or (\d+)\-(\d+)" [ name; r1x; r1y; r2x; r2y ] ->
            name,
            [ (int r1x, int r1y); (int r2x, int r2y) ]
        | x -> failwithf "Invalid field: %s" x)

let parseTicket input = String.split "," input |> List.map int
let parseYourTicket input = String.split "\n" input |> List.last |> parseTicket
let parseNearbyTickets input = String.split "\n" input |> List.tail |> List.map parseTicket

let notes =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.unpack3
    |> (fun (a, b, c) ->
        { Fields = parseFields a; YourTicket = parseYourTicket b; NearbyTickets = parseNearbyTickets c })

let isValidFieldValue field value =
    snd field |> List.fold (fun valid range -> valid || (value >= fst range && value <= snd range)) false

let isValidValue fields value =
    fields |> List.fold (fun valid field -> valid || isValidFieldValue field value) false

let errorRate fields tickets =
    tickets
    |> List.concat
    |> List.filter (isValidValue fields >> not)
    |> List.sum

let validTickets (fields: Field list) (tickets: Ticket list) =
    tickets |> List.filter (fun ticket -> ticket |> List.forall (isValidValue fields))

let fitsAt field (validTickets: Ticket list) i =
    validTickets
    |> List.map (fun ticket -> ticket.[i])
    |> List.fold (fun valid value -> valid && isValidFieldValue field value) true

let distributeFields (fields: Field list) validTickets =
    fields |> List.map (fun field ->
        field, [ 0 .. (fields.Length - 1) ] |> List.filter (fitsAt field validTickets))

let fitFields (fields: Field list) (validTickets) =
    [ 0 .. (fields.Length - 1) ]
    |> List.fold (fun (result: Map<int, Field>, distribution: (Field * int list) list) _ ->
        let field, indexes = distribution |> List.find (fun (_, indexes) -> indexes.Length = 1)
        result.Add(List.head indexes, field),
        distribution |> List.map (fun (field, indexes') -> field, List.except indexes indexes'))
        (Map.empty<int, Field>, distributeFields fields validTickets)
    |> fst

let part2 (yourTicket: Ticket) fields validTickets =
    fitFields fields validTickets
    |> Map.toList
    |> List.filter (fun (_, field) -> (fst field).Contains("departure"))
    |> List.map fst
    |> List.map (fun i -> int64 yourTicket.[i])
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (errorRate notes.Fields notes.NearbyTickets)
    printfn "Part 2: %A" (part2 notes.YourTicket notes.Fields (validTickets notes.Fields notes.NearbyTickets))
    0
