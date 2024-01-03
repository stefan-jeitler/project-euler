// solution inspired by: https://euler.stephan-brumme.com/50/

let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let primes = Seq.initInfinite (fun x -> x + 2) |> Seq.filter isPrime

let primeSums =
    primes
    |> Seq.indexed
    |> Seq.map (fun (i, x) -> primes |> Seq.take i |> Seq.sum)
    |> Seq.takeWhile (fun x -> x < 1_000_000)
    |> Seq.toList

let (maxPrimeIdx, maxPrimeSum) =
    primeSums
    |> Seq.indexed
    |> Seq.maxBy snd

let findMaxPrimeSum max = 
    let rec loop idx =
        let m = primeSums[maxPrimeIdx] - primeSums[idx]
        if idx >= maxPrimeIdx then
            None
        elif isPrime m then
            Some m
        else
            loop (idx + 1)

    loop 1

let result = findMaxPrimeSum maxPrimeSum