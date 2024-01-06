let isPalindrom (n: bigint) =
    let n' = string n
    let nReversed = n' |> Seq.rev |> System.String.Concat

    n' = nReversed

let reverse n =
    n
    |> string
    |> Seq.rev
    |> System.String.Concat
    |> System.Numerics.BigInteger.Parse

let isLychrelNumberInLessThen50Iterations n =
    let rec loop n i =
        let nReversed = reverse n
        let sum = n + nReversed

        if i = 50 then true
        elif isPalindrom sum then false
        else loop sum (i + 1)

    loop n 0

let result =
    { 1I .. 9_999I }
    |> Seq.filter isLychrelNumberInLessThen50Iterations
    |> Seq.length
