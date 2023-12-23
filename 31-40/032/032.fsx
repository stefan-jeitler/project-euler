let isPanDigital (a, b, c) =
    let a' = string a
    let b' = string b
    let c' = string c

    let x = String.concat "" [ a'; b'; c' ] |> Seq.sort |> System.String.Concat

    x = "123456789"

let multiplications =
    [ for i in [ 1..99 ] do
          for j in { 1..9999 } do
              yield (i, j, i * j) ]

let result =
    multiplications
    |> List.filter isPanDigital
    |> List.distinctBy (fun (_, _, c) -> c)
    |> List.sumBy (fun (_, _, c) -> c)
