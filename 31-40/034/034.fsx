let factorial n =
    if n <= 1UL then 1UL else { 1UL .. n } |> Seq.fold (*) 1UL

let (!) = factorial

let ``n times 9`` n =
    [ 1UL .. n ] |> List.fold (fun acc _ -> (acc * 10UL) + 9UL) 0UL

let sumOfFactorialsOfEachDigit n =
    let rec loop n =
        if n = 0UL then
            0UL
        else
            let next = n % 10UL
            !next + loop (n / 10UL)

    loop n

let limit =
    Seq.initInfinite (fun x -> uint64 (x + 1))
    |> Seq.map (fun x -> ``n times 9`` x)
    |> Seq.skipWhile (fun x -> x < (sumOfFactorialsOfEachDigit x))
    |> Seq.head

let result =
    { 3UL .. limit }
    |> Seq.filter (fun x -> sumOfFactorialsOfEachDigit x = x)
    |> Seq.sum
