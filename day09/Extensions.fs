module Extensions

module List =
    let apply (fList: ('a->'b) list) (xList: 'a list)  = 
        [ for f in fList do
          for x in xList do
              yield f x ]    
    
    let (<*>) = apply

    let (<!>) = List.map    
    
    let rec combinations n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs