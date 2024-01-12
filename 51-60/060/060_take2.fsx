// Simpler solution based on posts in the project euler forum

open System.Collections.Concurrent

let memoize f =
    let cache = ConcurrentDictionary<_, _>()

    fun c ->
        let exists, value = cache.TryGetValue c

        if exists then
            value
        else
            // optimistic concurrency issue here, but in this scenario it is acceptable to compute some values twice or more
            cache.GetOrAdd(c, valueFactory = f)

let isPrime n =
    if n <> 2 && n % 2 = 0 then
        false
    elif n <> 3 && n % 3 = 0 then
        false
    else
        let limit = float n |> sqrt |> ceil |> int
        seq { 5..2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let isPrime' = memoize isPrime

let estimatedLimit = 10_000
let firstNPrimes = { 2..estimatedLimit } |> Seq.filter isPrime |> Seq.toList

let goToNextPrimeGreaterThan x primes =
    primes |> List.skipWhile (fun p -> p <= x)

let digits n =
    if n = 0 then 1 else n |> float |> log10 |> ceil |> int

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
