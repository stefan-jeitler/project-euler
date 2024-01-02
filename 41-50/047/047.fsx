let rec calculatePrimeFactors product factor (acc: int list) =
    if factor > product then
        acc
    elif (product % factor = 0) then
        calculatePrimeFactors (product / factor) factor (factor :: acc)
    else
        calculatePrimeFactors product (factor + 1) acc

let primeFactors n = calculatePrimeFactors n 2 []

let hasAllFourConsecutivePrimeFactors window =
    let primeFactors = window |> Seq.map (fun x -> x, primeFactors x) |> Seq.toList

    let product f = f |> List.reduce (*)
    let distinctLength l = l |> List.distinct |> List.length

    primeFactors |> List.forall (fun (_, factors) -> factors |> distinctLength = 4)
    && primeFactors |> List.forall (fun (n, factors) -> product factors = n)

let result =
    Seq.initInfinite (fun x -> x + 1000)
    |> Seq.windowed 4
    |> Seq.skipWhile (hasAllFourConsecutivePrimeFactors >> not)
    |> (Seq.head >> Array.head)
