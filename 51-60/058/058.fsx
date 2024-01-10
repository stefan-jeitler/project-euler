
let isPrime n =
    if n < 2UL then
        false
    else
        let limit = n |> float |> sqrt |> uint64
        { 2UL .. limit } |> Seq.exists (fun x -> n % x = 0UL) |> not

let primesRatio totalPoints primePoints = float primePoints / float totalPoints

let primesOfSingleSquare (sideLength: int) =
    let steps = uint64 sideLength - 1UL
    let firstOfCurrentSquare = (max (pown (uint64 sideLength - 2UL) 2) 1UL) + steps
    let maxValue = pown (uint64 sideLength) 2

    [ firstOfCurrentSquare..steps..maxValue ]
    |> Seq.filter isPrime
    |> Seq.length

let sideLengthPrimeRatios =
    Seq.unfold
        (fun (sideLength, (totalPoints, primePoints)) ->
            let currentSideLength = sideLength + 2
            let primesOfCurrentSquare = primesOfSingleSquare currentSideLength
            let totalPoints = totalPoints + 4
            let primePoints = primePoints + primesOfCurrentSquare
            let current = currentSideLength, primesRatio totalPoints primePoints
            let acc = (sideLength + 2, (totalPoints, primePoints))
            Some(current, acc))
        (1, (1, 0))

sideLengthPrimeRatios
|> Seq.skipWhile (fun (_, ratio) -> ratio >= 0.1)
|> Seq.head
|> fst
