open System.IO
open Extensions

type Ticket = int list
type Range = int * int
type Field = string * Range list

type Notes =
    { Fields: Field list
      YourTicket: Ticket
      NearbyTickets: Ticket list }

let parseRange input =
    String.split "-" input
    |> List.map int
    |> List.unpack2

let parseFields input =
    input
    |> String.split "\n"
    |> List.map (fun line ->
        match line with
        | Regex "(.*): (.*) or (.*)" [ name; range1; range2 ] -> name, [ parseRange range1; parseRange range2 ]
        | x -> failwithf "Invalid field: %s" x)

let parseTicket input =
    input |> String.split "," |> List.map int

let parseYourTicket input =
    input
    |> String.split "\n"
    |> List.last
    |> parseTicket

let parseNearbyTickets input =
    input
    |> String.split "\n"
    |> List.tail
    |> List.map parseTicket

let notes =
    let (fieldsInput, ticketInput, nearbyInput) =
        File.ReadAllText("input.txt")
        |> String.split "\n\n"
        |> List.unpack3

    { Fields = parseFields fieldsInput
      YourTicket = parseYourTicket ticketInput
      NearbyTickets = parseNearbyTickets nearbyInput }

let isValidFieldValue field value =
    snd field
    |> List.fold (fun valid range ->
        valid
        || (value >= fst range && value <= snd range)) false

let isValidValue fields value =
    fields
    |> List.fold (fun valid field -> valid || isValidFieldValue field value) false

let errorRate fields tickets =
    tickets
    |> List.concat
    |> List.filter (isValidValue fields >> not)
    |> List.sum

let validTickets (fields: Field list) (tickets: Ticket list) =
    tickets
    |> List.filter (fun ticket -> ticket |> List.forall (isValidValue fields))

let isFieldAt i field (validTickets: Ticket list) =
    validTickets
    |> List.map (fun ticket -> ticket.[i])
    |> List.fold (fun valid value -> valid && isValidFieldValue field value) true

let fitFields (fields: Field list) (validTickets) =
    [ 0 .. (fields.Length - 1) ]
    |> List.map (fun i ->
        i, fields
        |> List.filter (fun field -> isFieldAt i field validTickets))


let apply f xs ys = 
    ys |> List.collect (fun y -> f xs y)    

let identifyFields (fields: Field list) (validTickets: Ticket list) =
    let fits = fitFields fields validTickets
    
    [ 0 .. (fits.Length -1 ) ]
    let matches =
        fits |> List.fold (fun result (i, fieldsAtIndex) ->
            apply (fun set y -> Set.add y set) result fieldsAtIndex 
            ) [Set.empty]
        |> List.filter (fun fields' -> fields' |> Set.count = fields.Length)
    
    matches |> List.head |> Set.toList
    
let departureFields fields validTickets =
    let identified = identifyFields fields validTickets

    identified
    |> List.indexed
    |> List.filter (fun (i, field) -> (fst field).Contains("departure"))
    |> List.map fst

let part2 (yourTicket: Ticket) fields validTickets =
    let fields = departureFields fields validTickets

    fields
    |> List.map (fun i -> yourTicket.[i])
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %A" (errorRate notes.Fields notes.NearbyTickets)
    printfn "Part 2: %A" (part2 notes.YourTicket notes.Fields (validTickets notes.Fields notes.NearbyTickets))
    0
