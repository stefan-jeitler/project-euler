let calculateSumOfSquares numbers =
    numbers
    |> Seq.map (fun x -> x * x)
    |> Seq.sum

let calculateSquareOfSum (numbers: int seq) = 
    let sum = numbers |> Seq.sum
    sum * sum

let first100NaturalNumbers = {1 .. 100}

let sumOfSquares = calculateSumOfSquares first100NaturalNumbers
let squareOfSum = calculateSquareOfSum first100NaturalNumbers


let result = abs (sumOfSquares - squareOfSum)
