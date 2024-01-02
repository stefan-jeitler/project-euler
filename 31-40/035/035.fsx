let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let allRotations n =
    let digitsCount = floor ((log10 (float n)) + 1.) |> int

    let rec loop x (acc: int list) =
        if x = n && acc.Length > 0 then
            acc
        else
            let firstDigit = x / (pown 10 (digitsCount - 1))
            let remaining = x % (pown 10 (digitsCount - 1))

            let next = (remaining * 10) + firstDigit

            loop next (next :: acc)

    loop n []

let allRotationsArePrimes n =
    let rotations = allRotations n

    rotations |> List.forall (fun x -> isPrime x)

let primesBelowOneMillion = { 2..1_000_000 } |> Seq.filter isPrime

let result = primesBelowOneMillion |> Seq.filter allRotationsArePrimes |> Seq.length
