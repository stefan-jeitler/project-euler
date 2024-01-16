// https://proofwiki.org/wiki/Continued_Fraction_Expansion_of_Euler%27s_Number
let eulersContinuedFractionExpansion =
    [| for i in { 0..2..200 } do
           yield! [ 1; i; 1 ] |]

let numerators (eulersContinuedFraction: int array) =
    let a = eulersContinuedFraction

    let p0 = 1I
    let p1 = 1I
    let i = 2

    let rec loop ``p n-2`` ``p n-1`` i =
        let p = (bigint a[i]) * ``p n-1`` + ``p n-2``

        seq {
            yield p
            yield! loop ``p n-1`` p (i + 1)
        }

    loop p0 p1 i

let sumOfDigits (n: bigint) =
    n |> string |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.sum

let result =
    eulersContinuedFractionExpansion
    |> numerators
    |> Seq.take 100
    |> Seq.last
    |> sumOfDigits
