let isPanDigitalOneToNine (n: uint64) =
    let n' = string n

    let x = n' |> Seq.sort |> System.String.Concat

    x = "123456789"

let concatenate (a: uint64) (b: uint64) =
    let digitsOfB = floor ((log10 (float b)) + 1.) |> int

    let intermediate = a * (pown 10UL digitsOfB)
    intermediate + b

let rec concatenatedProductOneToN n =

    let rec loop m acc =
        let next = concatenate acc (n * m)

        match next, m with
        | _, m when m >= n -> None
        | x, _ when x < 99_999_999UL -> loop (m + 1UL) x
        | x, _ when x > 99_999_999UL && x < 1_000_000_000UL && (isPanDigitalOneToNine x) -> Some x
        | _ -> None

    loop 1UL 0UL

let result = { 2UL .. 999_999UL } |> Seq.choose concatenatedProductOneToN |> Seq.max
