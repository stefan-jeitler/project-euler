open System.IO
let filePath = Path.Combine(__SOURCE_DIRECTORY__, "0059_cipher.txt")

let toBytes (s: string) = System.Text.Encoding.ASCII.GetBytes(s)

let toString (b: byte list) =
    System.Text.Encoding.ASCII.GetString(b |> List.toArray)

let repeatKey totalLength (key: string) =
    let keyLength = key.Length

    if keyLength >= totalLength then
        toBytes key[0..totalLength]
    else
        { 0 .. totalLength - 1 }
        |> Seq.map (fun i -> key[i % keyLength])
        |> Seq.map byte
        |> Seq.toArray

let encrypted =
    File.ReadAllText filePath |> _.Split(",") |> Seq.map (int >> byte) |> Seq.toList

let decrypt (encrypted: byte list) (key: string) =
    let encryptedLength = List.length encrypted
    let keyRepeated = repeatKey encryptedLength key |> Array.toList

    (encrypted, keyRepeated) ||> List.zip |> List.map (fun (a, b) -> a ^^^ b)

let upperCaseChars = [ 'A' .. 'Z' ]
let lowercaseCharacters = [ 'a' .. 'z' ]
let allCharacters = [ yield! lowercaseCharacters; yield! upperCaseChars ]

let possibleKeys =
    [ for a in lowercaseCharacters do
          for b in lowercaseCharacters do
              for c in lowercaseCharacters do
                  [ byte a; byte b; byte c ] ]

let isVowel (c: char) = "AEIOUaeiou".IndexOf(c) >= 0

let isConsonant (c: char) =
    allCharacters |> Seq.contains c && isVowel c |> not

let addCountOfConsonantFollowedByAVowelOccurrence (c: string) =
    c,
    c
    |> Seq.windowed 2
    |> Seq.filter (fun x -> isConsonant x[0] && isVowel x[1])
    |> Seq.length

let decryptedText =
    possibleKeys
    |> Seq.map (fun k -> decrypt encrypted (toString k))
    |> Seq.map toString
    // Words often consist of a consonant followed by a vowel
    |> Seq.map addCountOfConsonantFollowedByAVowelOccurrence
    |> Seq.maxBy snd
    |> fst

let result = decryptedText |> Seq.map int |> Seq.sum
