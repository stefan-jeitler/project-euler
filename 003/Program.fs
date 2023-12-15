let rec calculatePrimeFactors product factor (acc: uint64 list) =
    if factor > product then
        acc
    elif (product % factor = 0UL) then
        calculatePrimeFactors (product / factor) (factor + 1UL) (factor :: acc)
    else
        calculatePrimeFactors product (factor + 1UL) (acc)

let primeFactors n = calculatePrimeFactors n 2UL []

let result = primeFactors 600851475143UL
printfn $"largest prime factor is %A{result}"

