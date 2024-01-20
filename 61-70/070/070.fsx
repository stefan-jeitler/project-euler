#load "../../Utils.fsx"

open Utils

let isPermutation a b =
    if a = b then
        false
    else
        let a' = a |> string |> Seq.sort |> System.String.Concat
        let b' = b |> string |> Seq.sort |> System.String.Concat

        a' = b'

let upperBound = 10_000_000
let lower, upper = upperBound |> float |> sqrt |> ceil |> int |> fun x -> x - 1600, x + 800

let primes =
    Seq.initInfinite (fun x -> x + 2)
    |> Seq.filter isPrime
    |> Seq.skipWhile (fun x -> x <= lower)
    |> Seq.takeWhile (fun x -> x <= upper)
    |> Seq.toArray

let primePairs =
    [ for i, p1 in primes |> Array.indexed do
          for p2 in primes[i + 1 ..] do
              p1, p2 ]

let n (p1, p2) = p1 * p2

let phi (p1, p2) = (p1 - 1) * (p2 - 1)

let result =
    primePairs
    |> Seq.map (fun x -> n x, phi x)
    |> Seq.filter (fun (n, _) -> n <= upperBound)
    |> Seq.filter (fun (n, phi) -> isPermutation n phi)
    |> Seq.map (fun (n, phi) -> n, float n / float phi)
    |> Seq.minBy snd
    |> fst
