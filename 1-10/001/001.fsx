let numbers = [ 1..999 ]

let multipliers = [ 3; 5 ]

let isMultiple n m =
    match (n % m) with
    | 0 -> true
    | _ -> false

let rec findFirstMultiple (m: int list) n =
    match m with
    | [] -> None
    | head :: tail ->
        if (isMultiple n head) then
            Some n
        else
            (findFirstMultiple tail n)

let result =
    numbers
    |> List.map (findFirstMultiple multipliers)
    |> List.choose id
    |> List.sum
