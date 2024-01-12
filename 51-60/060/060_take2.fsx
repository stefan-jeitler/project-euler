// Simpler solution based on posts in the project euler forum

#load "../../Utils.fsx"
open Utils

let isPrime' = memoize isPrime

let estimatedLimit = 10_000
let firstNPrimes = { 2..estimatedLimit } |> Seq.filter isPrime |> Seq.toList

let goToNextPrimeGreaterThan x primes =
    primes |> List.skipWhile (fun p -> p <= x)

let concat left right =
    let rightDigits = digits right

    (left * pown 10 rightDigits) + right

let hasSpecialProperty a b =
    let ab = concat a b
    let ba = concat b a

    isPrime' ab && isPrime' ba

let hasAllSpecialProperty (found: int list) (candidate: int) =
    found |> List.forall (hasSpecialProperty candidate)

let result =
    seq {
        for a in firstNPrimes do
            for b in firstNPrimes |> goToNextPrimeGreaterThan a do
                if hasSpecialProperty a b then
                    for c in firstNPrimes |> goToNextPrimeGreaterThan b do
                        if hasAllSpecialProperty [ a; b ] c then
                            for d in firstNPrimes |> goToNextPrimeGreaterThan c do
                                if hasAllSpecialProperty [ a; b; c ] d then
                                    for e in firstNPrimes |> goToNextPrimeGreaterThan d do
                                        if hasAllSpecialProperty [ a; b; c; d ] e then
                                            a + b + c + d + e

    }
    |> Seq.min
