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

open System.Collections.Concurrent

let memoize f =
    let dict = ConcurrentDictionary<_, _>()

    fun c ->
        let exist, value = dict.TryGetValue c

        match exist with
        | true -> value
        | _ ->
            // still not thread safe, but acceptable in this scenario to compute some values twice or more often
            dict.GetOrAdd(c, valueFactory = f)

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

let satisfy a b =
    let ab = concat a b
    let ba = concat b a

    isPrime' ab && isPrime' ba

let satisfyAll (found: int list) (candidate: int) =
    found |> List.forall (satisfy candidate)

let rec findLongestChain start primes =
    let rec aggregateSingleSlice (remainingPrimes: int list) acc =
        if remainingPrimes.IsEmpty then
            acc
        else
            let found = remainingPrimes |> List.tryFind (fun x -> satisfyAll acc x)

            match found with
            | Some v -> aggregateSingleSlice (remainingPrimes |> List.skipWhile (fun x -> x <= v)) (v :: acc)
            | None -> acc

    let rec loop s p =
        match p with
        | [] -> seq { [] }
        | x ->
            seq {
                yield (aggregateSingleSlice x [ start ])
                yield! (loop s x[1..])
            }

    loop start primes |> Seq.maxBy _.Length

let combinations (allPrimes: int list) length =
    let rec loop (primes: int list) (acc: Async<int list option> seq) =
        match primes with
        | [] -> acc
        | head :: tail ->
            seq {
                let computeSinglePrime =
                    async {
                        let longestChain = findLongestChain head tail

                        return
                            if longestChain.Length = length then
                                Some longestChain
                            else
                                None
                    }

                yield!
                    loop
                        tail
                        (seq {
                            yield computeSinglePrime
                            yield! acc
                        })
            }

    loop allPrimes []

// copied from: https://www.compositional-it.com/news-blog/improved-asynchronous-support-in-f-4-7/
module Async =
    /// A wrapper around the throttling overload of Parallel to allow easier pipelining.
    let ParallelThrottle throttle workflows = Async.Parallel(workflows, throttle)

let result =
    combinations firstNPrimes 5
    |> Async.ParallelThrottle System.Environment.ProcessorCount
    |> Async.RunSynchronously
    |> Seq.choose id
    |> Seq.map List.sum
    |> Seq.min
