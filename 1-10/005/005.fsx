let isEvenlyDvisible range number =
    range |> List.forall (fun x -> number % x = 0)

let result =
    Seq.initInfinite id
    |> Seq.skip 20
    |> Seq.find (isEvenlyDvisible [2 .. 20])

