let rec inserts x l =
    seq {
        match l with
        | [] -> yield [ x ]
        | y :: rest ->
            yield x :: l

            for i in inserts x rest do
                yield y :: i
    }

let rec permutations l =
    seq {
        match l with
        | [] -> yield []
        | x :: rest ->
            for p in permutations rest do
                yield! inserts x p
    }

let firstSevenPrimes = [ 2; 3; 5; 7; 11; 13; 17 ]
let panDigits = [ '0' .. '9' ]

let hasSliceDivisibilityProperty (slice: char list) (prime: int) =
    let number = slice |> List.map string |> System.String.Concat |> int

    number % prime = 0

let hasSubstringDivisibilityProperty (digits: char list) =
    firstSevenPrimes
    |> List.indexed
    |> List.forall (fun (i, x) -> hasSliceDivisibilityProperty digits[i + 1 .. i + 3] x)

let toUint64 (l: char list) =
    l |> List.map string |> System.String.Concat |> uint64

let result =
    permutations panDigits
    |> Seq.filter hasSubstringDivisibilityProperty
    |> Seq.map toUint64
    |> Seq.sum
