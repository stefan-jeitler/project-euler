let continuedFraction s =
    let m0 = 0I
    let d0 = 1I
    let a0 = float s |> sqrt |> bigint

    let rec loop m d a =
        let mx = d * a - m
        let dx = (s - (mx * mx)) / d
        let ax = (a0 + mx) / dx

        seq {
            yield ax
            yield! loop mx dx ax
        }

    seq {
        yield a0
        yield! loop m0 d0 a0
    }

let continuedFractionConvergents n =
    let a = continuedFraction n |> Seq.take 1000 |> Seq.toArray

    let p0 = 1I
    let p1 = a[0]
    let q0 = 0I
    let q1 = 1I

    let rec loop ``p n-2`` ``p n-1`` ``q n-2`` ``q n-1`` n =
        let an = a[n]
        let px = an * ``p n-1`` + ``p n-2``
        let qx = an * ``q n-1`` + ``q n-2``

        seq {
            yield px, qx
            yield! loop ``p n-1`` px ``q n-1`` qx (n + 1)
        }

    seq {
        yield p1, q1
        yield! loop p0 p1 q0 q1 1
    }

let perfectSquares = [ 2I .. 32I ] |> Seq.map (fun x -> pown x 2) |> Set

let satisfiesDiophantineEquation (d: bigint) (x: bigint) (y: bigint) = x * x - d * y * y = 1I

let result =
    { 2I .. 1000I }
    |> Seq.filter (perfectSquares.Contains >> not)
    |> Seq.map (fun d ->
        d,
        continuedFractionConvergents d
        |> Seq.find (fun (x, y) -> satisfiesDiophantineEquation d x y))
    |> Seq.maxBy (snd >> fst)
    |> fst
