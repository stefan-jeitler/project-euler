#load "../../Utils.fsx"

open Utils

let firstNPrimes n =
    Seq.initInfinite id |> Seq.filter isPrime |> Seq.take n |> Seq.toList

let first100Primes = firstNPrimes 100

let rec count n (m: int list) =
    match n, m with
    | n, m when n < 0 || m.Length = 0 -> 0
    | n, _ when n = 0 -> 1
    | n, m ->
        let next = m |> List.head
        let remaining = m |> List.skip 1
        (count n remaining) + (count (n - next) m)

let sumOfPrimes n =
    let primesOfInterest = first100Primes |> List.takeWhile (fun x -> x < n)
    count n primesOfInterest

let result =
    Seq.initInfinite (fun x -> x + 2)
    |> Seq.skipWhile (fun x -> sumOfPrimes x < 5000)
    |> Seq.head
