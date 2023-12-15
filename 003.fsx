let rec calculatePrimeFactors product factor acc =
    if factor = product then
        factor :: acc
    elif product % factor = 0UL then
        calculatePrimeFactors (product / factor) factor (factor :: acc)
    else
        calculatePrimeFactors product (factor + 1UL) acc

let maxPrimeFactor n = calculatePrimeFactors n 2UL []
let result = maxPrimeFactor 600851475143UL |> List.max
