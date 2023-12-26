let isPrime n =
    if n < 2UL then
        false
    else
        let limit = n |> float |> sqrt |> uint64
        { 2UL .. limit } |> Seq.exists (fun x -> n % x = 0UL) |> not

let removeDigits remover n =
    let rec loop n acc =
        if n = 0UL then
            acc
        else
            let digitRemoved = remover n
            loop digitRemoved (n :: acc)

    loop n []

let removeDigitsLeft =
    let leftRemover n =
        let digitsCount = (floor (log10 (float n)) |> int |> (+) 1)
        n % (pown 10UL (digitsCount - 1))

    removeDigits leftRemover

let removeDigitsRight = removeDigits (fun x -> x / 10UL)

let isSpecialPrime n =
    let digitsTruncated = removeDigitsLeft n
    let digitsTruncatedReversed = removeDigitsRight n

    isPrime n
    && digitsTruncated |> List.forall isPrime
    && digitsTruncatedReversed |> List.forall isPrime

let limit = System.UInt64.MaxValue

let result =
    { 11UL .. limit } |> Seq.filter isSpecialPrime |> Seq.take 11 |> Seq.sum
