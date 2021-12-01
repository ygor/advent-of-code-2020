module Extensions

module CyclicList =
    let removeManyAt index count list =
        let length = List.length list
        let tailCount = List.min [count; count - (index + count - length)]
        let headCount = List.max [0; (index + count - length)]
        
        list
        |> List.removeManyAt index tailCount
        |> List.removeManyAt 0 headCount

    let takeManyAt (index: int) (count: int) list =
        (list @ list).[index .. (index + count - 1)]