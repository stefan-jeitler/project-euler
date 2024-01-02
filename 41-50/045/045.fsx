let isPentagonal (n: uint64) =
    // quadratic equation
    let x1 = (1. + sqrt (1. + 24. * float n)) / 6.

    x1 % 1. = 0

let hexagonal (n: uint64) = n * (2UL * n - 1UL)

let result =
    Seq.unfold (fun c -> Some(hexagonal c, c + 1UL)) 144UL
    // https://en.wikipedia.org/wiki/Hexagonal_number: Every hexagonal number is a triangular number
    // -> no need to check triangular numbers
    |> Seq.skipWhile (fun x -> not (isPentagonal x))
    |> Seq.head
