let isPalindrom (n: string) =
    let nAsStringReversed = n |> Seq.rev |> System.String.Concat

    n = nAsStringReversed

let toBinary (n: int) =
    System.Convert.ToString(n, 2).TrimStart('0')

let result =
    { 1..999_999 }
    |> Seq.filter (fun x -> (isPalindrom (string x)) && (isPalindrom (toBinary x)))
    |> Seq.sum
