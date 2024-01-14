
let result =
    [ 1. .. 9. ]
    |> List.map (fun x -> (1. / (1. - log10 x)) |> floor |> int)
    |> List.sum
