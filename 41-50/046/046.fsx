let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let isOddComposite n =
    if n < 4 || n % 2 = 0 then false else n |> isPrime |> not

let oddComposites = Seq.initInfinite (fun x -> x + 4) |> Seq.filter isOddComposite

let satisfiesGoldbachsConjecture n =
    Seq.initInfinite (fun x -> x + 1)
    |> Seq.map (fun x -> n - 2 * x * x)
    |> Seq.takeWhile (fun x -> x <= n)
    |> Seq.exists isPrime

let result = oddComposites |> Seq.find (satisfiesGoldbachsConjecture >> not)
