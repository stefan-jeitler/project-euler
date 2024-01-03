let rec inserts x l =
    seq {
        match l with
        | [] -> yield [ x ]
        | y :: rest ->
            yield x :: l

            for i in inserts x rest do
                yield y :: i
    }

let rec permutations l =
    seq {
        match l with
        | [] -> yield []
        | x :: rest ->
            for p in permutations rest do
                yield! inserts x p
    }

let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let estimatedLimit = 1_000_000
let primes = { 2..estimatedLimit } |> Seq.filter isPrime |> Set

let digits n =
    if n = 0 then
        1
    else
        n |> float |> log10 |> floor |> int |> (+) 1

let place n i r =
    let placementAtRightPlace = r * pown 10 i
    let placementWithRemaingingDigits = placementAtRightPlace + (n % (pown 10 i))

    let x = pown 10 (i + 1)
    (n / x) * x + placementWithRemaingingDigits

let replace (number: int) pattern (replacements: int list) =
    let indices =
        pattern
        |> Seq.rev
        |> Seq.indexed
        |> Seq.filter (fun (i, x) -> x = 'X')
        |> Seq.map fst
        |> Seq.toList

    let replaceAll x =
        x |> Seq.fold (fun acc (i, r) -> place acc i r) number

    replacements
    |> Seq.map (fun r -> indices |> List.map (fun i -> (i, r)))
    |> Seq.map replaceAll
    |> Seq.toList

let generatePatterns digits =
    let initialPattern = System.String('0', digits)

    let toString chars =
        chars |> List.map string |> System.String.Concat

    [ 0 .. digits - 1 ]
    |> Seq.map (fun i ->
        initialPattern
            .Insert(0, System.String('X', i + 1))
            .Remove(initialPattern.Length))
    |> Seq.map (Seq.toList >> permutations)
    |> Seq.collect id
    |> Seq.map toString
    |> Seq.distinct
    |> Seq.toList

let patterns = [ 1..9 ] |> List.map (fun x -> x, generatePatterns x) |> Map

let findFirstFamilies prime nPrimes =
    let digitsCount = digits prime
    let lowerBound = pown 10 (digitsCount - 1)

    let result =
        patterns[digitsCount]
        |> Seq.map (fun p -> p, (replace prime p [ 0..9 ]))
        |> Seq.map (fun (p, x) -> p,
                                            x
                                            |> Seq.filter (fun x -> x >= lowerBound)
                                            |> Seq.filter (fun x -> primes.Contains x)
                                            |> Seq.toList)
        |> Seq.filter (fun (p, x) -> x.Length = nPrimes)    
        |> Seq.toList

    prime, result

let result =
    primes
    |> Seq.map (fun x -> findFirstFamilies x 8)
    |> Seq.filter (fun (_, r) -> r.Length <> 0)
    |> Seq.take 1
    |> Seq.head
    |> (snd >> List.head >> snd)
    |> List.min
