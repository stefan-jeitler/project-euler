let ceiling = 4_000_000

let createNextTillCeiling ((first, second): int * int) =
    let nextFibonacciNumber = first + second

    if (nextFibonacciNumber <= ceiling) then
        Some(nextFibonacciNumber, (second, nextFibonacciNumber))
    else
        None

let result =
    Seq.unfold createNextTillCeiling (0, 1)
    |> Seq.filter (fun x -> x % 2 = 0 )
    |> Seq.sum
