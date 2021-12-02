module Extensions
    
module List =
    let repeat n fn = List.init n (fun _ -> fn) |> List.reduce (>>)
    
module Map =
    let removeMany keys map =
        keys |> List.fold (fun map' key -> Map.remove key map') map

    
