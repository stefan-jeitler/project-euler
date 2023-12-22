let isPalindrom c =
    let cAsString = string c
    let cAsStringReversed = cAsString |> Seq.rev |> System.String.Concat

    cAsString = cAsStringReversed

let largestPalindromOf value =
    let numbers = { 1..value }

    numbers
    |> Seq.collect (fun x -> numbers |> Seq.map (fun y -> x * y))
    |> Seq.filter isPalindrom
    |> Seq.max

let result = largestPalindromOf 999
