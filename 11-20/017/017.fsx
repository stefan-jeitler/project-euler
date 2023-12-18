open System.Text.RegularExpressions

let words =
    Map
        [ 0, ""
          1, "one"
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


let translateThousands n = 
    match n / 1000 with
    | x when x > 0 && x < 10 -> $"%s{words[x]} %s{words[1000]}"
    | _ -> ""

let translateHundreds n = 
    let h =  n / 100 % 10
    if h > 0 then
        $" %s{words[h]} %s{words[100]}"
    else ""

let translateLastTwoDigits n = 
    match n % 100 with
    | x when x <= 20 -> $" %s{words[x]}"
    | x -> $" %s{words[x / 10 * 10]} %s{words[x % 10]}"

let translate number = 
    let thousands = translateThousands number
    let hundreds = translateHundreds number
    let lastTwoDigits = translateLastTwoDigits number

    let addAnd = if number > 99 then " and" else ""

    (thousands + hundreds + addAnd + lastTwoDigits)


let result =
    { 1..1000 }
    |> Seq.map translate
    |> Seq.map (fun x -> Regex.Replace(x, "\s+", ""))
    |> Seq.map _.Length
    |> Seq.sum
