// pretty much the same as euler 31

let numbers = [ 99..-1..1 ]

let rec count n (m: int list) =
    match n, m with
    | n, m when n < 0 || m.Length = 0 -> 0
    | n, _ when n = 0 -> 1
    | n, m ->
        let next = m |> List.head
        let remaining = m |> List.skip 1
        (count n remaining) + (count (n - next) m)

let result = count 100 numbers
