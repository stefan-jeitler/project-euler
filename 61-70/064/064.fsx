// based on: https://en.wikipedia.org/wiki/Periodic_continued_fraction#Canonical_form_and_repetend
let recurringExpansionLength s =
    let m0 = 0
    let d0 = 1
    let a0 = float s |> sqrt |> int

    let rec loop m d a (acc: int list) =

        match acc with
        | x when x.Length > 0 && x.Head = 2 * a0 -> x.Length        
        | _ ->
            let m' = d * a - m
            let d' = (s - (m' * m')) / d
            let a' = (a0 + m') / d' |> int
            loop m' d' a' (a'::acc)

    loop m0 d0 a0 []

let perfectSquares = 
    [ 2 .. 100 ]
    |> Seq.map (fun x -> pown x 2)
    |> Set

let isOdd n = n % 2 <> 0

let result =
    [ 2 .. 10_000 ]
    |> Seq.filter (perfectSquares.Contains >> not)
    |> Seq.map recurringExpansionLength
    |> Seq.filter isOdd
    |> Seq.length
