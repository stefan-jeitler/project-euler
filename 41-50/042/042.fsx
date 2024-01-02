let isInteger n = (n % 1.) < System.Double.Epsilon

open System.IO
let filePath = Path.Combine(__SOURCE_DIRECTORY__, "0042_words.txt")

let words =
    File.ReadAllText filePath
    |> _.Split(",")
    |> Array.map _.Trim('\"')
    |> Array.toList

let alphabeticalValueByChar =
    [ 'A' .. 'Z' ] |> List.map (fun x -> (x, int x - 64)) |> dict

let wordValue c =
    c |> Seq.map (fun x -> alphabeticalValueByChar[x]) |> Seq.sum

let isTriangleNumber (n: int) =
    // quadratic equation
    let x1 = (sqrt (1. + 8. * float n) - 1.) / 2. 

    isInteger x1

let result =
    words |> List.map wordValue |> List.filter isTriangleNumber |> List.length
