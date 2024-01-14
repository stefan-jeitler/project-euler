#load "Common.fsx"

open Common

let polygonalTypeConstraints =
    [ isTriangular; isSquare; isPentagonal; isHexagonal; isHeptagonal; isOctagonal ]

let fourDigitNumbersOfInterest =
    [ 1010..9999 ]
    |> List.filter (fun x -> polygonalTypeConstraints |> List.exists (fun f -> f x))
    |> List.filter (fun x -> lastTwoDigitsOf x > 9)

let cycles =
    seq {
        for a in fourDigitNumbersOfInterest do
            for b in fourDigitNumbersOfInterest do
                if lastTwoDigitsOf a = firstTwoDigitsOf b then
                    for c in fourDigitNumbersOfInterest do
                        if lastTwoDigitsOf b = firstTwoDigitsOf c then
                            for d in fourDigitNumbersOfInterest do
                                if lastTwoDigitsOf c = firstTwoDigitsOf d then
                                    for e in fourDigitNumbersOfInterest do
                                        if lastTwoDigitsOf d = firstTwoDigitsOf e then
                                            for f in fourDigitNumbersOfInterest do
                                                if
                                                    lastTwoDigitsOf e = firstTwoDigitsOf f
                                                    && lastTwoDigitsOf f = firstTwoDigitsOf a
                                                then
                                                    [ a; b; c; d; e; f ]
    }

type ConstraintsResult =
    | SomeMatch of (int -> bool) list
    | Unmatched

let satisfy constraints numbers =

    let rec tryMatchSingleConstraint constraints n (acc: (int -> bool) list) =
        match constraints with
        | [] -> Unmatched
        | f :: fs ->
            if f n then
                SomeMatch(acc @ fs)
            else
                tryMatchSingleConstraint fs n (f :: acc)


    let rec loop numbers constrains =
        match numbers with
        | [] -> true
        | current :: rest ->
            match tryMatchSingleConstraint constrains current [] with
            | SomeMatch remaining -> loop rest remaining
            | Unmatched -> false

    loop numbers constraints

let satisfyTypeConstraints = satisfy polygonalTypeConstraints

let result =
    cycles 
    |> Seq.filter (fun x -> satisfyTypeConstraints x) 
    |> Seq.map List.sort
    |> Seq.distinct
    |> Seq.collect id
    |> Seq.sum
