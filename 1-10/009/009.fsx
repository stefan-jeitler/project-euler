let f x = x |> float

let isPythagoreanTriple (a: int, b: int, c': int) =
    let ``c^2`` = ((f a) ** 2.) + (f b) ** 2.
    let c = sqrt ``c^2``
    let cAsInt = int c

    not (c <> cAsInt) && not (cAsInt <> c') && (a < b && b < c')

let generateTriples =
    seq {
        for a in { 1..998 } do
            for b in { 1..998 } do
                for c in { 1..998 } do
                    if (a + b + c) = 1000 then
                        yield (a, b, c)
    }

let result =
    generateTriples
    |> Seq.filter isPythagoreanTriple
    |> Seq.map (fun (a, b, c) -> a * b * c)
    |> Seq.head
