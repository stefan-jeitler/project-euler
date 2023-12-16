let findDivisors (n: uint64) =
    let rec innerFn (d: uint64) acc =
        let limit = n |> float |> sqrt |> uint64

        if d > limit then
            acc
        elif n % d = 0UL then
            let accumulator =
                if n / d <> d then
                   d :: ((n / d) :: acc ) 
                else 
                   d :: acc

            innerFn (d + 1UL) accumulator
        else
            innerFn (d + 1UL) acc

    innerFn 2UL [ 1UL; n ]

let result =
    Seq.initInfinite (fun x -> (uint64 x) + 1UL)
    |> Seq.map (fun x -> { 1UL .. x } |> Seq.sum)
    |> Seq.map findDivisors
    |> Seq.skipWhile (fun x -> x.Length < 500)
    |> Seq.head
    |> List.max
