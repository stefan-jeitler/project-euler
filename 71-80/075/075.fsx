#load "../../Utils.fsx"
open Utils

let perimeters upperBound =
    seq {
        for m in [ 2 .. upperBound / 2 |> (float >> sqrt >> int) ] do
            let ``m²`` = m * m

            for n in { 1 .. m - 1 } do
                if (m + n) % 2 <> 0 && gcd m n = 1 then
                    let ``n²`` = n * n

                    let a = ``m²`` - ``n²``
                    let b = 2 * m * n
                    let c = ``m²`` + ``n²``

                    let perimeter = a + b + c

                    if perimeter <= upperBound then
                        yield perimeter

                        let rec loop n =
                            let a' = n * a
                            let b' = n * b
                            let c' = n * c
                            let perimeter' = a' + b' + c'

                            seq {
                                if perimeter' <= upperBound then
                                    yield perimeter'
                                    yield! loop (n + 1)
                            }

                        yield! loop 2
    }

let result =
    perimeters 1_500_000
    |> Seq.groupBy id
    |> Seq.filter (fun (k, v) -> Seq.length v = 1)
    |> Seq.map fst
    |> Seq.length
