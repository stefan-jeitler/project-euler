let isEven n = n % 2UL = 0UL

let next n =
    if isEven n then n / 2UL else 3UL * n + 1UL

let generate start =
    let rec innerFn n acc =
        if n = 1UL then n :: acc else innerFn (next n) (n :: acc)

    innerFn start []

let result =
    { 1UL .. 1_000_000UL }
    |> Seq.map (fun x -> x, (generate x))
    |> Seq.map (fun (n, s) -> n, s.Length)
    |> Seq.maxBy snd

let (number, length) = result
