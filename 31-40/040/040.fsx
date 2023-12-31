let fraction =
    let rec loop n =
        seq {
            yield! n |> string
            yield! loop (n + 1)
        }

    loop 1

let nthElementsOfInterest = [ 1; 10; 100; 1_000; 10_000; 100_000; 1_000_000 ] |> set

let result =
    fraction
    |> Seq.take 1_000_000
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> nthElementsOfInterest.Contains(i + 1))
    |> Seq.map (snd >> System.Char.GetNumericValue >> int)
    |> Seq.reduce (*)
