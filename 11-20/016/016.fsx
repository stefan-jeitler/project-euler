open System

let ``base`` = 2.
let exponent = 1000

let result =
    sprintf "%.0F" (``base`` ** exponent)
    |> Seq.map (Char.GetNumericValue >> uint64)
    |> Seq.sum
