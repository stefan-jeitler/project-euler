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

let primesGenerator = Seq.initInfinite (fun x -> x + 2) |> Seq.filter isPrime
let estimatedLimit = 1_000_000
let primes = primesGenerator |> Seq.takeWhile (fun x -> x <= estimatedLimit) |> Set

let digits n =
    if n = 0 then
        1
    else
        n |> float |> log10 |> floor |> int |> (+) 1

// replace digit at i in number n with replacement r
let replaceDigit n i r =
    // e.g. replaceDigit 472 1 5 => 452
    // a => 400
    // b =>  50
    // c =>   2

    let x = pown 10 (i + 1)
    let a = (n / x) * x

    let b = r * pown 10 i
    let c = (n % (pown 10 i))
    
    a + b + c

let replace (number: int) pattern (replacements: int list) =
    let indices =
        pattern
        |> Seq.rev
        |> Seq.indexed
        |> Seq.filter (fun (i, x) -> x = 'X')
        |> Seq.map fst
        |> Seq.toList

    let replaceAll x =
        x |> Seq.fold (fun acc (i, r) -> replaceDigit acc i r) number

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

let patterns = [ 1..6 ] |> List.map (fun x -> x, generatePatterns x) |> Map

let familiesWithNPrimes nPrimes prime =
    let digitsCount = digits prime
    let lowerBound = pown 10 (digitsCount - 1)

    let result =
        patterns[digitsCount]
        |> Seq.map (fun pattern -> pattern, (replace prime pattern [ 0..9 ]))
        |> Seq.map (fun (p, r) ->
            p,
            r
            |> Seq.filter (fun x -> x >= lowerBound)
            |> Seq.filter (fun x -> primes.Contains x)
            |> Seq.toList)
        |> Seq.filter (fun (p, x) -> x.Length = nPrimes)
        |> Seq.toList

    match result with
    | [] -> None
    | r -> Some(prime, r)

let familiesWith8Primes = familiesWithNPrimes 8
let firstFamily = primesGenerator |> Seq.choose familiesWith8Primes |> Seq.head
let result = firstFamily |> snd |> List.collect snd |> List.min
