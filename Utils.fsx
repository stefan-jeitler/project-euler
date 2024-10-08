module Utils

open System.Collections.Concurrent

// copied from: https://www.compositional-it.com/news-blog/improved-asynchronous-support-in-f-4-7/
module Async =
    /// A wrapper around the throttling overload of Parallel to allow easier pipelining.
    let ParallelThrottle throttle workflows = Async.Parallel(workflows, throttle)

let memoize f =
    let cache = ConcurrentDictionary<_, _>()

    fun c ->
        let exists, value = cache.TryGetValue c

        if exists then
            value
        else
            // optimistic concurrency issue here, but in most scenarios it is acceptable to compute some values twice or more
            cache.GetOrAdd(c, valueFactory = f)

let isPrime n =
    if n < 2 then false
    elif n <> 2 && n % 2 = 0 then
        false
    elif n <> 3 && n % 3 = 0 then
        false
    else
        let limit = float n |> sqrt |> ceil |> int
        seq { 5..2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let rec private inserts x l =
    seq {
        match l with
        | [] -> yield [ x ]
        | y :: rest ->
            yield x :: l

            for i in inserts x rest do
                yield y :: i
    }

let rec permute l =
    seq {
        match l with
        | [] -> yield []
        | x :: rest ->
            for p in permute rest do
                yield! inserts x p
    }

let rec gcd a b =
    match a, b with
    | (a, 0) -> a
    | (a, b) -> gcd b (a % b)