let findDivisors (n: uint64) =
    let rec innerFn (d: uint64) acc =
        let limit = d |> float |> sqrt |> uint64

        if d > limit then acc
        elif n % d = 0UL then innerFn (d + 1UL) ([ d; n / d ] @ acc)
        else innerFn (d + 1UL) acc

    innerFn 2UL [ 1UL; n ]


findDivisors 10UL
10UL |> float |> sqrt |> uint64

let triangleNumbers =
    Seq.initInfinite (fun x -> (uint64 x) + 1UL)
    |> Seq.map (fun x -> { 1UL .. x } |> Seq.sum)
    |> Seq.map findDivisors
    |> Seq.skipWhile (fun x -> x.Length < 500)
    |> Seq.head
    |> List.max
