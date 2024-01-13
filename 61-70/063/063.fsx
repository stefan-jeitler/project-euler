
let digits n = if n = 0. then 1 else n |> log10 |> int |> (+) 1

let countOfNDigitNumbers (exponent: int) =
    let bases = 
        Seq.initInfinite float
        |> Seq.skip 1

    bases
    |> Seq.map (fun b -> b ** exponent)
    |> Seq.map (fun p -> p, digits p)
    |> Seq.skipWhile (fun (_, digitsCount) -> digitsCount < exponent)
    |> Seq.takeWhile (fun (_, digitsCount) -> digitsCount = exponent)
    |> Seq.length

let result = 
    Seq.initInfinite id
    |> Seq.skip 1
    |> Seq.map countOfNDigitNumbers
    |> Seq.takeWhile (fun x -> x > 0)
    |> Seq.sum

