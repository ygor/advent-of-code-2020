module Extensions

module Map =
  let keys (m: Map<'Key, 'T>) =
    Map.fold (fun keys key _ -> key::keys) [] m
    
module Array2D =
    let toSeq array =
        seq {
            for r in 0 .. (Array2D.length1 array - 1) do
                for c in 0 .. (Array2D.length2 array - 1) do
                    yield (r, c), array.[r, c]
        }

module List =
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]    
    
    let (<*>) = apply

    let (<!>) = List.map        

    let unpack2 list =
        match list with
        | [ a; b ] -> a, b
        | _ -> failwithf "Tried to unpack2 list without exactly 2 elements: %A" list