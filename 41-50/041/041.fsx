let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let isPanDigital (n: int) =
    let n' = string n
    let panDigitsOneToN = [ 1 .. n'.Length ] |> List.map string |> System.String.Concat
    let x = n' |> Seq.sort |> System.String.Concat

    x = panDigitsOneToN

let sumOfDigits n =
    n |> string |> Seq.map (System.Char.GetNumericValue >> int) |> Seq.sum

let limit =
    [ 987654321; 87654321; 7654321; 654321; 54321 ]
    // https://en.wikipedia.org/wiki/Divisibility_rule#Divisibility_by_3_or_9
    |> List.skipWhile (fun x -> (sumOfDigits x) % 3 = 0)
    |> List.head

let result =
    { 2143..limit }
    |> Seq.rev
    |> Seq.filter isPrime
    |> Seq.filter isPanDigital
    |> Seq.head
