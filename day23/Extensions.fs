module Extensions

module Func =
    let repeat n repeater state =
        [ 1 .. n ] |> List.fold (fun state' _ -> repeater state') state

module Map =
    let removeMany keys map =
        keys
        |> List.fold (fun map' key -> Map.remove key map') map

    let addMany keyValues map =
        keyValues
        |> List.fold (fun map' (key, value) -> Map.add key value map') map
