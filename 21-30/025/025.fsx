let fib =
    let rec loop a b =
        seq {
            if a = 0I then
                yield 1I

            let next = a + b
            yield next
            yield! loop b next
        }

    loop 0I 1I

let digitsCount n = n |> string |> _.Length

let result =
    fib 
    |> Seq.indexed
    |> Seq.skipWhile (fun (_, x) -> (digitsCount x) < 1000)
    |> Seq.head
    |> fst
    |> (+) 1 // one-based index
