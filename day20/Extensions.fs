module Extensions

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray

module List =    
    let unique list = list |> Set.ofList |> List.ofSeq
    
module Fun =
    let repeat n fn =
        match n with
        | 0 -> id
        | _ -> List.init n (fun _ -> fn) |> List.reduce (>>)
