module Extensions

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray