module Common

#load "../../Utils.fsx"

open Utils

let isInteger n = (n % 1.) = 0.

let isTriangular y =
    let x1 = (sqrt (1. + 8. * float y) - 1.) / 2.

    isInteger x1

let isSquare a = a |> float |> sqrt |> isInteger

let isPentagonal y =
    let x1 = (1. + sqrt (1. + 24. * float y)) / 6.

    isInteger x1

let isHexagonal y =
    let x1 = (1. + sqrt (1. + 8. * float y)) / 4.

    isInteger x1

let isHeptagonal y =
    let x1 = (1.5 + sqrt (2.25 + 10. * float y)) / 5.

    isInteger x1

let isOctagonal y =
    let x1 = (2. + sqrt (4. + 12. * float y)) / 6.

    isInteger x1

let lastTwoDigitsOf n = n % 100

let firstTwoDigitsOf n =
    let digits = digits n

    if n < 10 then n else n / (pown 10 (digits - 2))
