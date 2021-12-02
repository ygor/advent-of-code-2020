module Extensions

module Map =
    let merge (p: Map<'a,'b>) (q: Map<'a,'b>) = 
        Seq.concat [ (Map.toSeq q); (Map.toSeq p) ] |> Map.ofSeq    

module Seq =
    let lengthBy filter list =
        list
        |> Seq.filter filter
        |> Seq.length
        
module Func =
    let repeat n repeater state =
        [ 1 .. n ] |> List.fold (fun state' _ -> repeater state') state
