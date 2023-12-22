let isPrime n =
    if n < 2UL then
        false
    else
        let limit = n |> float |> sqrt |> uint64
        { 2UL .. limit } |> Seq.exists (fun x -> n % x = 0UL) |> not

let infiniteUint64Seq (start: uint64) =
    let rec innerFn n =
        seq {
            yield n
            yield! innerFn (n + 1UL)
        }

    innerFn start

let result =
    infiniteUint64Seq 2UL |> Seq.filter isPrime |> Seq.skip 10_000 |> Seq.head
