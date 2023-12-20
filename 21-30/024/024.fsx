let rec inserts x l =
    seq {
        match l with
        | [] -> yield [ x ]
        | y :: rest ->
            yield x :: l

            for i in inserts x rest do
                yield y :: i
    }

let rec permutations l =
    seq {
        match l with
        | [] -> yield []
        | x :: rest ->
            for p in permutations rest do
                yield! inserts x p
    }

let decimals = [ '0' .. '9' ]

permutations decimals
|> Seq.map List.toArray
|> Seq.map (fun x -> System.String x)
|> Seq.sort
|> Seq.skip 999_999
|> Seq.head
