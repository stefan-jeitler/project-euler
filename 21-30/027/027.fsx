let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let f a b n = (n * n) + (a * n) + b

let range =
    [ for i in { -999 .. 999 } do
          for j in { -999 .. 999 } do
              yield (i, j) ]

let lengthOfConsecutivePrimes (a, b) =
    Seq.initInfinite id |> Seq.takeWhile (fun x -> isPrime (f a b x)) |> Seq.length

let result =
    range
    |> List.mapi (fun i x -> i, lengthOfConsecutivePrimes x)
    |> List.maxBy snd
    |> fst
    |> fun i -> range[i]
    |> fun (a, b) -> a * b
