let isPrime n =
    if n < 2UL then
        false
    else
        let limit = n |> float |> sqrt |> uint64
        { 2UL .. limit } 
        |> Seq.exists (fun x -> n % x = 0UL)
        |> not

let result = 
    { 2UL .. 2_000_000UL - 1UL }
    |> Seq.filter isPrime
    |> Seq.sum
    