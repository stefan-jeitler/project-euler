#load "../../Utils.fsx"
open Utils

let estimatedLimit = 10_000
let firstNPrimes = { 2..estimatedLimit } |> Seq.filter isPrime |> Seq.toList

let isPrime' = memoize isPrime

let digits n = if n = 0 then 1 else n |> float |> log10 |> int |> (+) 1

let concat left right =
    let rightDigits = digits right

    (left * pown 10 rightDigits) + right

let hasSpecialProperty a b =
    let ab = concat a b
    let ba = concat b a

    isPrime' ab && isPrime' ba

let hasAllSpecialProperty (found: int list) (candidate: int) =
    found |> List.forall (hasSpecialProperty candidate)

let goToNextPrimeGreaterThan x primes =
    primes |> List.skipWhile (fun p -> p <= x)

let rec findLongestChainOfPrimesWithSpecialProperty start primes =
    let rec findChain (remainingPrimes: int list) acc =
        if remainingPrimes.IsEmpty then
            acc
        else
            let found = remainingPrimes |> List.tryFind (fun x -> hasAllSpecialProperty acc x)

            match found with
            | Some v -> findChain (remainingPrimes |> goToNextPrimeGreaterThan v) (v :: acc)
            | None -> acc

    let rec loop p =
        match p with
        | [] -> seq { [] }
        | x ->
            seq {
                yield (findChain x [ start ])
                yield! (loop x[1..])
            }

    loop primes |> Seq.maxBy _.Length

let createComputationForSinglePrime prime remainingPrimes wantedLength =
    async {
        let longestChain = findLongestChainOfPrimesWithSpecialProperty prime remainingPrimes

        return
            if longestChain.Length = wantedLength then
                Some longestChain
            else
                None
    }

let combinations (allPrimes: int list) length =
    let rec loop (primes: int list) =
        match primes with
        | [] -> seq { async.Return None }
        | head :: tail ->
            seq {
                yield createComputationForSinglePrime head tail length
                yield! loop tail
            }

    loop allPrimes

let result =
    combinations firstNPrimes 5
    |> Async.ParallelThrottle System.Environment.ProcessorCount
    |> Async.RunSynchronously
    |> Seq.choose id
    |> Seq.map List.sum
    |> Seq.min
