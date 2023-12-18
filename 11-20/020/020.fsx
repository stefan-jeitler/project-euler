let factorial n =
    if n = bigint 0 then 1I 
    else { 1I .. n } |> Seq.fold (*) 1I

let (!) = factorial

let result =
    ! 100I
    |> string
    |> Seq.map (System.Char.GetNumericValue >> int)
    |> Seq.sum
