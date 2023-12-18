let d n =
    seq {
        let limit = n / 2UL

        yield 1UL
        yield! { 2UL..limit } |> Seq.filter (fun x -> n % x = 0UL)
    } |> Seq.sum

let isAmicable a = 
    let b = d a
    let a' = d b

    a' = a && a' <> b


let result =
    { 2UL .. 10_000UL }
    |> Seq.filter isAmicable
    |> Seq.sum