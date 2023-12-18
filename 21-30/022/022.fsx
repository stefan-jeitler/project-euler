open System.IO
let filePath = Path.Combine(__SOURCE_DIRECTORY__, "names.txt")
let names = 
    File.ReadAllText filePath
    |> _.Split(",")
    |> Seq.map _.Trim('\"')
    |> Seq.toList


let alphabeticalValueByChar = 
    ['A' .. 'Z']
    |> List.map (fun x -> (x, int x - 64))
    |> dict

let calcAlphabeticalValue c =
    c 
    |> Seq.map (fun x -> alphabeticalValueByChar[x]) 
    |> Seq.sum

let result = 
    names
    |> Seq.sort
    |> Seq.mapi (fun i x -> (i + 1) * calcAlphabeticalValue x)
    |> Seq.sum
