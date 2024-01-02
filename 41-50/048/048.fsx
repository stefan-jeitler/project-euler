let selfPower (n: int) =
    pown (bigint n) n

let takeLastTenDigits (n: bigint) = n % 10_000_000_000I

let result =
    { 1..1000 }
    |> Seq.map selfPower
    |> Seq.sum
    |> takeLastTenDigits