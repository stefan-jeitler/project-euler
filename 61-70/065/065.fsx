// https://proofwiki.org/wiki/Continued_Fraction_Expansion_of_Euler%27s_Number
let eulersContinuedFraction count : int array =

    let rec loop counter n =
        seq {
            if counter = 2 then
                yield n
                yield! loop 0 (n + 2)
            else
                yield 1
                yield! loop (counter + 1) n
        }

    seq {
        yield 1
        yield 0
        yield! loop 0 2
    }
    |> Seq.take count
    |> Seq.toArray

let expansionNumerators (eulersContinuedFraction: int array) =
    let a = eulersContinuedFraction

    let p0 = 1I
    let p1 = 1I

    let rec loop p'' p' i =
        let p = (bigint a[i]) * p' + p''

        seq {
            yield p
            yield! loop p' p (i + 1)
        }

    loop p0 p1 2

let sumOfDigits (n: bigint) =
    n |> string |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.sum

let result =
    (eulersContinuedFraction 102)
    |> expansionNumerators
    |> Seq.take 100
    |> Seq.last
    |> sumOfDigits
