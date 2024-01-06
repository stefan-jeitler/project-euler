let pairs =
    [ for a in [ 1I..99I ] do
          for b in [ 1..99 ] do
              a, b ]

let sumOfDigits n =
    n |> string |> Seq.map (string >> System.Numerics.BigInteger.Parse) |> Seq.sum

let result =
    pairs
    |> Seq.map (fun (a, b) -> pown a b)
    |> Seq.map sumOfDigits
    |> Seq.max
