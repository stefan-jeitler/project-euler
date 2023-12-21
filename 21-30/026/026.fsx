let isPrime n =
    if n < 2 then
        false
    else
        let limit = n |> float |> sqrt |> int
        { 2..limit } |> Seq.exists (fun x -> n % x = 0) |> not

let rec gcd a b =
    match a, b with
    | (a, 0) -> a
    | (a, b) -> gcd b (a % b)

let multiplicativeOrder a n =
    let d = gcd a n

    if d <> 1 then
        None
    else
        let rec loop kn r =
            let result = a * r % n
            if result = 1 then kn + 1 else loop (kn + 1) rest

        let result = loop 1 a

        match result with
        | r when r < 1 -> None
        | r -> Some r

let multiplicativeOrder10 = multiplicativeOrder 10

let isFullReptendPrime n =
    if not (isPrime n) then
        false
    else
        match multiplicativeOrder10 n with
        | Some p when p = (n - 1) -> true
        | _ -> false

let result = [ 2..999 ] |> List.filter isFullReptendPrime |> List.max
