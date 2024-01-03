let factorial n =
    if n <= 1. then 1. else { 1. .. n } |> Seq.fold (*) 1.

let (!) = factorial

let binomial n k =
    if k > n then 0. else !n / (!k * !(n - k))

let pairs =
    [ for n in [ 1..100 ] do
          for k in [ 1..n ] do
              n, k ]

let result =
    pairs
    |> List.map (fun (n, k) -> binomial n k)
    |> List.filter (fun x -> x > 1_000_000)
    |> List.length
