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

let t (n: int) =
    let n' = float n
    (n' / 2.) * (n' + 1.)

let isTriangleNumber (n: int) =

    let rec loop i =
        let x = t i

        match x with
        | x when x < n -> loop (i + 1)
        | x when x = n -> true
        | _ -> false

    loop 1

let result =
    words |> List.map wordValue |> List.filter isTriangleNumber |> List.length
