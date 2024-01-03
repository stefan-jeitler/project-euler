let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let primes = Seq.initInfinite (fun x -> x + 2) |> Seq.filter isPrime

let limit = 1_000_000

let primeSums =
    Seq.initInfinite id
    |> Seq.indexed
    |> Seq.map (fun (i, _) -> primes |> Seq.take i |> Seq.sum)
    |> Seq.takeWhile (fun x -> x < limit)
    |> Seq.toList

let consecutivePrimeSummands sum idx =
    let rec loop fi s i =
        if fi >= i then
            s, 0
        elif (isPrime (primeSums[i])) then
            primeSums[i], i
        elif (isPrime (primeSums[i] - primeSums[fi])) then
            primeSums[i] - primeSums[fi], i - fi
        else
            loop (fi + 1) primeSums[i] i

    loop 0 sum idx

let result =
    primeSums
    |> Seq.indexed
    |> Seq.map (fun (i, s) -> consecutivePrimeSummands s i)
    |> Seq.maxBy snd
    |> fst
