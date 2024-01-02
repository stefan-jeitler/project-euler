let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let isPermutation a b =
    if a = b then
        false
    else
        let a' = a |> string |> Seq.sort |> System.String.Concat
        let b' = b |> string |> Seq.sort |> System.String.Concat

        a' = b'

let primesOfInterest = { 9999..-1..1001 } |> Seq.filter isPrime |> Seq.toList

let calcDifferencesBetweenItems l =
    [ for (i, x) in l |> List.indexed do
          for y in l[i + 1 ..] do
              ((x, y), y - x) ]

let tripleWithSpecialProperty x =
    let candidate =
        calcDifferencesBetweenItems x
        |> List.groupBy (fun (_, diff) -> diff)
        |> List.filter (fun (_, v) -> v.Length = 2)
        |> List.map (fun (_, v) -> v |> List.collect (fun ((a, b), diff) -> [ a; b ]) |> List.distinct)
        |> List.collect id

    match candidate with
    | x when x.Length = 3 -> Some(x[0], x[1], x[2])
    | _ -> None

let (<|<) n digits = (uint64 n) * pown 10UL digits

let result =
    primesOfInterest
    |> List.map (fun x ->
        primesOfInterest
        |> List.filter (isPermutation x)
        |> List.collect (fun y -> [ x; y ]))
    |> Seq.map (fun x -> x |> List.distinct |> List.sort)
    |> Seq.filter (fun x -> x.Length >= 3)
    |> Seq.distinct
    |> Seq.filter (fun x -> x |> List.contains 1487 |> not)
    |> Seq.choose tripleWithSpecialProperty
    |> Seq.head
    |> (fun (a, b, c) -> (a <|< 8) + ((b <|< 4) + uint64 c))
