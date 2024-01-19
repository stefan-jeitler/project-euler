#load "../../Utils.fsx"

open Utils

let first20Primes = 
    Seq.initInfinite (fun x -> x + 2)
    |> Seq.filter isPrime
    |> Seq.take 20
    |> Seq.toList

let upperBound = 1_000_000

let result =
    let rec loop remainingPrimes acc =
        match remainingPrimes with
        | [] -> failwith "not enough primes provided"
        | head :: _ when acc * head > upperBound -> acc
        | head :: tail ->
            loop tail (head * acc)

    loop first20Primes 1