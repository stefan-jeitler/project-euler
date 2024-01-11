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

module Seq =
    let append item sequence =
        seq {
            yield! sequence
            yield item
        }

// copied from: https://www.compositional-it.com/news-blog/improved-asynchronous-support-in-f-4-7/
module Async =
    /// A wrapper around the throttling overload of Parallel to allow easier pipelining.
    let ParallelThrottle throttle workflows = Async.Parallel(workflows, throttle)

let isPrime n =
    if n <> 2 && n % 2 = 0 then
        false
    elif n <> 3 && n % 3 = 0 then
        false
    else
        let limit = float n |> sqrt |> ceil |> int
        seq { 5..2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let estimatedLimit = 10_000
let firstNPrimes = { 2..estimatedLimit } |> Seq.filter isPrime |> Seq.toList

let isPrime' = memoize isPrime

let digits n =
    if n = 0 then
        1
    else
        n |> float |> log10 |> floor |> int |> (+) 1

let digits' = memoize digits

let concat left right =
    let rightDigits = digits' right

    (left * pown 10 rightDigits) + right

let hasSpecialProperty a b =
    let ab = concat a b
    let ba = concat b a

    isPrime' ab && isPrime' ba

let hasAllSpecialProperty (found: int list) (candidate: int) =
    found |> List.forall (hasSpecialProperty candidate)

let goToNextPrimeGreaterThan x primes = primes |> List.skipWhile (fun p -> p <= x)

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
    let rec loop (primes: int list) (acc: Async<int list option> seq) =
        match primes with
        | [] -> acc
        | head :: tail ->
            seq {
                let computation = createComputationForSinglePrime head tail length
                yield! loop tail (Seq.append computation acc)
            }

    loop allPrimes []

let result =
    combinations firstNPrimes 5
    |> Async.ParallelThrottle System.Environment.ProcessorCount
    |> Async.RunSynchronously
    |> Seq.choose id
    |> Seq.map List.sum
    |> Seq.min
