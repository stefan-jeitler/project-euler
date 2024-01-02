let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let primesBelowOneMillion = [ 7..999_999 ] |> List.filter isPrime

let calcLengthOfConsecutivePrimeSummands prime =
    let rec loop p (acc: int * int list) =
        let (sum, summands) = acc

        if sum = prime then
            Some summands
        else
            match p with
            | head :: _ when head >= prime && sum > 0 -> None
            | head :: tail when sum + head <= prime -> loop tail ((sum + head), head :: summands)
            | _ -> None

    let summands = loop primesBelowOneMillion (0, [])
    summands |> Option.map (fun s -> prime, s.Length)

let result =
    primesBelowOneMillion
    |> List.choose calcLengthOfConsecutivePrimeSummands
    |> List.maxBy snd
