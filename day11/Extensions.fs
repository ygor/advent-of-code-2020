module Extensions
    
module Array2D =
    let toSeq array =
        seq {
            for r in 0 .. (Array2D.length1 array - 1) do
                for c in 0 .. (Array2D.length2 array - 1) do
                    yield (r, c), array.[r, c]
        }
