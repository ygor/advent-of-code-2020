module Extensions
    
module List =
    let rec cartesian list =
        match list with
        | h::[] ->
            List.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            List.fold (fun acc' elem' ->
                (List.fold (fun acc elem -> (elem::elem')::acc) [] h) @ acc') [] (cartesian t)
        | _ -> []
        
module Tuple =
    let sum (a, b) = a + b    
