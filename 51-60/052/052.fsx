let containsSameDigits a b =
    let a' = a |> string |> Seq.sort |> Seq.map string |> System.String.Concat
    let b' = b |> string |> Seq.sort |> Seq.map string |> System.String.Concat

    a' = b'

let multiples = [ 1..6 ]

let containsAllSameDigits (l: int list) =
    l |> List.windowed 2 |> List.forall (fun x -> containsSameDigits x[0] x[1])

let result =
    Seq.initInfinite (fun x -> x + 1)
    |> Seq.map (fun x -> multiples |> List.map (fun y -> x * y))
    |> Seq.skipWhile (containsAllSameDigits >> not)
    |> Seq.head
    |> List.min
