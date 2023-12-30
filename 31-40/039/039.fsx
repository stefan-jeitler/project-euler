let isInteger n = (n % 1.) < System.Double.Epsilon

let perimeters =
    [ for a in [ 2..332 ] do
          for b in { 3..(1000 - 2 * a) } do
              let c = sqrt ((float a ** 2) + (float b ** 2))
              let p = a + b + int c

              if isInteger c && p <= 1000 then
                  p ]

let result =
    perimeters
    |> List.groupBy id
    |> List.maxBy (fun (k, v) -> v.Length)
    |> fst
