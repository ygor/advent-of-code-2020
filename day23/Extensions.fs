module Extensions

module Map =
    let removeMany keys map =
        keys |> List.fold (fun map' key -> Map.remove key map') map
