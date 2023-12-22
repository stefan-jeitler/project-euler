// based on: https://algorithmist.com/wiki/Coin_change

let coins = [ 200; 100; 50; 20; 10; 5; 2; 1 ]

let rec count n (m: int list) =
    match n, m with
    | n, m when n < 0 || m.Length <= 0 -> 0
    | n, _ when n = 0 -> 1
    | n, m ->
        let nextCoin = m |> List.head
        let remainingCoins = m |> List.skip 1
        (count n remainingCoins) + (count (n - nextCoin) m)

let result = count 200 coins
