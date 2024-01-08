let expansion length =
    let rec loop i n d =
        if i = length then
            n, d
        else
            loop (i + 1) (2I * d + n) (d + n)

    loop 0 1I 1I

let digits n = n |> string |> _.Length

let numeratorExceedsDenominatorDigits (n, d) = digits n > digits d

let result =
    { 1..1000 }
    |> Seq.map expansion
    |> Seq.filter numeratorExceedsDenominatorDigits
    |> Seq.length
