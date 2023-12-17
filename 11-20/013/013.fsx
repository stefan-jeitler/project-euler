open System
open System.IO

let filePath = Path.Combine(__SOURCE_DIRECTORY__, "100FiftyDigitNumbers.txt")
let fiftyDigitNumbers = File.ReadAllLines filePath

let splitDigits n =
    string n |> Seq.toList |> List.map (Char.GetNumericValue >> int)

let sum numbers =

    let rec innerFn (n: string list) (sumDigits: int list) carry =
        if n |> List.isEmpty then
            match carry with
            | 0 -> sumDigits
            | _ -> ((splitDigits carry) @ sumDigits)
        else
            let splitted = n |> List.map (fun x -> (x[0], x[1..]))
            let nextDigits = splitted |> List.map fst

            let remainingDigits =
                match splitted |> List.map snd with
                | head :: _ when head = "" -> []
                | x -> x

            let sumOfCurrentSlice =
                nextDigits
                |> List.map Char.GetNumericValue
                |> List.reduce (+)
                |> int
                |> (+) carry

            let newSumDigit = sumOfCurrentSlice % 10
            let newCarry = sumOfCurrentSlice / 10
            innerFn remainingDigits (newSumDigit :: sumDigits) newCarry

    innerFn numbers [] 0

let result =
    fiftyDigitNumbers
    |> List.ofArray
    |> List.map (fun x -> x |> Seq.rev |> Seq.map string |> String.concat "")
    |> sum
    |> List.take 10
    |> List.map string
    |> List.reduce (+)
