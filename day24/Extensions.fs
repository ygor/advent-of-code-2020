module Extensions

module Map =
    let join (p: Map<'a,'b>) (q: Map<'a,'b>) = 
        Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ] |> Map.ofSeq    

module Func =
    let repeat n repeater state =
        [ 1 .. n ] |> List.fold (fun state' _ -> repeater state') state
