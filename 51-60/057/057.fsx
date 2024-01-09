let expansions  =
    let rec loop n d =
        seq {
            yield (n, d)
            yield! loop (2I * d + n) (d + n)
        }

    loop 1I 1I

let digits n = n |> string |> _.Length

let result =
    expansions
    |> Seq.take 1000
    |> Seq.filter (fun (n, d) -> digits n > digits d)
    |> Seq.length
