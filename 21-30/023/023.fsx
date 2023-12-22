let sumOfDivisors =
    function
    | 1UL -> 1UL
    | n -> { 1UL .. (n / 2UL) } |> Seq.filter (fun x -> n % x = 0UL) |> Seq.sum

let isAbundant n = sumOfDivisors n > n

let totalRange = [ 1UL .. 28123UL ]

let abundantNumbers = totalRange |> List.filter isAbundant

let sumOfAllAbundantNumbers =
    abundantNumbers
    |> Seq.collect (fun a -> abundantNumbers |> Seq.map (fun b -> a + b))
    |> set

let result =
    totalRange
    |> List.filter (fun x -> not (sumOfAllAbundantNumbers.Contains x))
    |> List.sum
