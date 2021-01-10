module Extensions

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray

module List =
    let unique list = list |> Set.ofList |> List.ofSeq

module Matrix =
    let rec transpose =
        function
        | ((x :: xs) :: ys) as matrix ->
            List.map List.head matrix
            :: transpose (List.map List.tail matrix)
        | _ -> []
    
    let rotate<'a> : 'a list list -> 'a list list =
        transpose >> List.rev
        
module Array2D =
    let toList array =
        [ 0 .. (Array2D.length2 array) - 1 ]
        |> Seq.fold (fun list' y ->
            [ 0 .. (Array2D.length1 array) - 1 ]
            |> Seq.fold (fun list x ->
                if x = 0 then
                    list @ [ [ array.[x, y] ] ]
                else
                    list.[0..(list.Length - 2)]
                    @ [ List.last list @ [ array.[x, y] ] ]) list') []

    let fromList (list: 'a list list) =
        if list.IsEmpty
        then Array2D.zeroCreate 0 0
        else Array2D.init (list.Head.Length) list.Length (fun x y -> list.[y].[x])

module Fun =
    let repeat n fn =
        match n with
        | 0 -> id
        | _ -> List.init n (fun _ -> fn) |> List.reduce (>>)
