open System.Text.RegularExpressions

let words =
    Map
        [ 1, "one"
          2, "two"
          3, "three"
          4, "four"
          5, "five"
          6, "six"
          7, "seven"
          8, "eight"
          9, "nine"
          10, "ten"
          11, "eleven"
          12, "twelve"
          13, "thirteen"
          14, "fourteen"
          15, "fifteen"
          16, "sixteen"
          17, "seventeen"
          18, "eighteen"
          19, "nineteen"
          20, "twenty"
          30, "thirty"
          40, "forty"
          50, "fifty"
          60, "sixty"
          70, "seventy"
          80, "eighty"
          90, "ninety"
          100, "hundred"
          1000, "thousand" ]

let ``and`` = "and"

open System

let wordify number =
    if number > 9999 then
        failwith "Numbers greater than 9999 are not supported"

    let numberAsString = string number

    let getSingleDigitAsInt idx =
        numberAsString[idx] |> Char.GetNumericValue |> int

    let parseThousandSignificance idx =
        let n = getSingleDigitAsInt idx
        let n = words[n]

        $"%s{n} %s{words[1000]}"

    let parseHundredSignificance idx =
        let n = getSingleDigitAsInt idx

        if n = 0 then "" else $"%s{words[n]} %s{words[100]}"

    let parseDecadeSignificance idx =
        let n = numberAsString[idx..] |> int
        let addAnd =
            match numberAsString.Length with
            | 1 -> ""
            | 2 when idx = 0 -> ""
            | _ -> ``and``
            
        match n with
        | 0 -> ""
        | _ when n <= 20 -> $"%s{addAnd} %s{words[n]}"
        | _ ->
            let tenSignificance = (getSingleDigitAsInt idx) * 10
            let oneSignificance =
                match getSingleDigitAsInt (idx + 1) with
                | 0 -> ""
                | x -> words[x]

            $"%s{addAnd} %s{words[tenSignificance]} %s{oneSignificance}"

    let isLastIndex idx = numberAsString.Length - 1 = idx
    
    let parseSingleDigit idx acc =
        let digits = max (numberAsString.Length - idx - 1) 1
        let significance = 10. ** digits |> int
        
        if isLastIndex idx && number > 9 then acc
        else
            match significance with
            | 1000 -> $"%s{acc} %s{parseThousandSignificance idx}"
            | 100 -> $"%s{acc} %s{parseHundredSignificance idx}"
            | 10 -> $"%s{acc} %s{parseDecadeSignificance idx}"
            | _ -> failwith "Something went badly wrong"

    let rec innerFn index acc =
        if index >= numberAsString.Length then
            acc
        else
            innerFn (index + 1) (parseSingleDigit index acc)

    innerFn 0 ""

let result =
    [ 1..1000 ]
    |> List.map wordify
    |> List.map (fun x -> Regex.Replace(x, "\s+", ""))
    |> List.map _.Length
    |> List.sum
