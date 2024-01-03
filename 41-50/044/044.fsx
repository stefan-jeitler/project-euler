let isPentagonal n =
    // quadratic equation
    let x1 = (1. + sqrt (1. + 24. * float n)) / 6.

    x1 % 1. = 0

let pentagonal n = n * (3 * n - 1) / 2

let pentagonalPairs =
    seq {
        for j in (Seq.initInfinite (fun x -> x + 2)) do
            for k in ({ j - 1 .. -1 .. 1 } |> Seq.rev) do
                yield (pentagonal j, pentagonal k)
    }

let isSumPentagonal (pj, pk) = isPentagonal (pj + pk)
let isDiffPentagonal (pj, pk) = isPentagonal (pj - pk)

let result =
    pentagonalPairs
    |> Seq.skipWhile (fun x -> not ((isSumPentagonal x) && (isDiffPentagonal x)))
    |> Seq.head
    |> fun (pj, pk) -> (pj - pk)
