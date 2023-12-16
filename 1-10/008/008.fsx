open System
open System.IO
open System.Text.RegularExpressions

let (/>) a b = Path.Combine(a, b)

let cwd = Directory.GetCurrentDirectory()
let filePath = cwd /> "1-10" /> "008" /> "1000Digits.txt"

let singleLineTrimmed t = Regex.Replace(t, "\s+", "").Trim()
let manyDigits = File.ReadAllText(filePath) |> singleLineTrimmed

let productOfDigits (digits: string) = 
    digits
    |> Seq.map (Char.GetNumericValue >> uint64)
    |> Seq.fold (fun acc c -> acc * c) 1UL

let result = 
    manyDigits  
    |> Seq.windowed 13
    |> Seq.map (String >> productOfDigits)
    |> Seq.max