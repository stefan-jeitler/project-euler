let isPermutation a b =
    if a = b then
        false
    else
        let a' = a |> string |> Seq.sort |> System.String.Concat
        let b' = b |> string |> Seq.sort |> System.String.Concat

        a' = b'

let findPermutations (numberWithSameDigitsCount: uint64 list) =
    let rec loop (numbers: uint64 list) (acc: uint64 list list) =
        match numbers with
        | [] -> 
            acc 
            |> Seq.groupBy _.Length
            |> Seq.maxBy fst
            |> snd

        | head :: tail ->
            let permutations = 
                tail 
                |> Seq.filter (fun x -> isPermutation head x)
                |> Seq.toList

            loop tail ((head :: permutations) :: acc)

    loop numberWithSameDigitsCount []

// range found with trial and error
let start = 5000UL
let end' = 9000UL

let result = 
    Seq.unfold (fun i -> Some (i, i + 1UL)) 1UL
    |> Seq.skipWhile (fun x -> x < start)
    |> Seq.takeWhile (fun x -> x <= end')
    |> Seq.map (fun x -> pown x 3)
    |> Seq.toList
    |> findPermutations
    |> Seq.map Seq.min
    |> Seq.min