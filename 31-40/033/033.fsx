let (>=>) a b = a |> Option.bind b

let findFirstSharedDigit n d =
    let d' = string d
    let firstDigit = n / 10
    let secondDigit = n % 10

    match firstDigit, secondDigit with
    | x, _ when x <> 0 && d'.Contains(string x) -> Some x
    | _, y when y <> 0 && d'.Contains(string y) -> Some y
    | _ -> None

let removeDigit n d =
    let first = n / 10
    let second = n % 10

    if first = d then second
    elif second = d then first
    else n

let fractions =
    [ for n in [ 10..99 ] do
          for d in [ 10..99 ] do
              if n < d then
                  yield (n, d) ]

let calcCuriousFraction (n, d) =
    let q = (float n) / (float d)
    let sharedDigit = findFirstSharedDigit n d

    sharedDigit >=> (fun x ->
        let n = removeDigit n x |> float
        let d = removeDigit d x |> float

        if (n < d) && (n / d) = q then Some q else None
    ) 

let result =
    fractions |> List.choose calcCuriousFraction |> List.reduce (*)

// 0.01 => 1/100
