// solution based on: https://raw.org/puzzle/project-euler/problem-72/
let upperBound = 1_000_000
let phi = [| 0UL .. uint64 upperBound |]

for i in [ 2..upperBound ] do
    if phi[i] = uint64 i then
        let rec loop j =
            if j <= upperBound then
                let i' = uint64 i
                phi[j] <- phi[j] / i' * (i' - 1UL)

                loop (j + i)

        loop i

let result = phi |> Seq.skip 2 |> Seq.sum
