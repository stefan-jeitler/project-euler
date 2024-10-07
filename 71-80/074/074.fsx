#load "../../Utils.fsx"

open Utils
open System

let factorial n =
    if n <= 1 then 1 else { 1..n } |> Seq.fold (*) 1

let sumOfFactorialsOfDigits n =
    string n
    |> Seq.map (Char.GetNumericValue >> int)
    |> Seq.map (fun x -> factorial x)
    |> Seq.sum

let sumOfFactorialsOfDigits' = memoize sumOfFactorialsOfDigits

let firstNonRepeatingTermsCount n =
    let rec loop next (acc: int list) =
        let next' = sumOfFactorialsOfDigits' next

        if acc.Length > 60 then
            None
        else if acc |> List.exists (fun x -> x = next') then
            Some acc.Length
        else
            loop next' (next' :: acc)

    loop n [ n ]

let result =
    [ 2..1_000_000 ]
    |> Seq.choose firstNonRepeatingTermsCount
    |> Seq.filter (fun x -> x = 60)
    |> Seq.length
