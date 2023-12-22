// based upon: https://stackoverflow.com/a/2200323/13842370

let factorial n =
    if n <= 1. then 1. else { 1. .. n } |> Seq.fold (*) 1.

let (!) = factorial


let binom n k =
    if k > n then 0. else !n / (!k * !(n - k))

let steps = 40.
let result = binom steps 20. |> uint64
