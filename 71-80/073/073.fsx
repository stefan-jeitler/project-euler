let fareySequence n =
    let p0 = 0
    let q0 = 1
    let p1 = 1
    let q1 = n

    let rec loop ``p n-2`` ``q n-2`` ``p n-1`` ``q n-1`` =
        seq {
            if not (``p n-2`` = 1 && ``q n-2`` = 1) then
                let c = (``q n-2`` + n) / ``q n-1``
                let p = c * ``p n-1`` - ``p n-2``
                let q = c * ``q n-1`` - ``q n-2``

                yield ``p n-1``, ``q n-1``
                yield! loop ``p n-1`` ``q n-1`` p q
        }

    seq {
        yield p0, q0
        yield! loop p0 q0 p1 q1
    }


let ``1/3`` = 1. / 3.
let ``1/2`` = 1. / 2.

let result =
    fareySequence 12_000
    |> Seq.skipWhile (fun (n, d) -> (float n / float d) <= ``1/3``)
    |> Seq.takeWhile (fun (n, d) -> (float n / float d) < ``1/2``)
    |> Seq.length
