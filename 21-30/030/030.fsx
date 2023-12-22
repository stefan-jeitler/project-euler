let sumOfFifthPowersOfDigits n =
    n
    |> string
    |> Seq.map (System.Char.GetNumericValue >> int)
    |> Seq.map (fun x -> pown x 5)
    |> Seq.sum

let ``n times 9`` n =
    [ 1..n ] |> List.fold (fun acc _ -> (acc * 10) + 9) 0

let limit =
    Seq.initInfinite id
    |> Seq.skip 1
    |> Seq.map (fun x -> x * (pown 9 5), ``n times 9`` x)
    |> Seq.skipWhile (fun (a, b) -> a > b)
    |> Seq.head
    |> fst

let isSumOfFifthPowersOfDigitsEqualToNumber n =
    let sum = sumOfFifthPowersOfDigits n
    n = sum

let result =
    [ 2..limit ] |> List.filter isSumOfFifthPowersOfDigitsEqualToNumber |> List.sum
