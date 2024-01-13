#load "../../Utils.fsx"
#load "Common.fsx"

open Utils
open Common

type PolygonalType =
    | Triangle
    | Square
    | Pentgonal
    | Hexagonal
    | Heptagonal
    | Octagonal

type Accumulator =
    { Types: PolygonalType list
      Values: int list list }

let numbersOfInterest =
    [ Triangle, [ 1010..9999 ] |> List.filter isTriangular
      Square, [ 1010..9999 ] |> List.filter isSquare
      Pentgonal, [ 1010..9999 ] |> List.filter isPentagonal
      Hexagonal, [ 1010..9999 ] |> List.filter isHexagonal
      Heptagonal, [ 1010..9999 ] |> List.filter isHeptagonal
      Octagonal, [ 1010..9999 ] |> List.filter isOctagonal ]
    |> List.sortBy (snd >> List.length)

// The 6th number must close the circle
let closeCircleAt = 6

let createAccumulator left right =
    let values =
        [ for l in (snd left) do
              for r in (snd right) do
                  if lastTwoDigitsOf l = firstTwoDigitsOf r then
                      [ l; r ] ]

    { Types = [ fst left; fst right ]
      Values = values }

let leftJoin (acc: int list) candidates =
    let first = acc[0]
    let last = acc |> List.last

    let requiresCycle = (acc.Length + 1) = closeCircleAt

    let isCyclicToFirst last =
        lastTwoDigitsOf last = firstTwoDigitsOf first

    let next =
        candidates
        |> List.tryFind (fun x ->
            lastTwoDigitsOf last = firstTwoDigitsOf x
            && (not requiresCycle || isCyclicToFirst x))

    match next with
    | Some n -> [ yield! acc; n ]
    | None -> acc

let addToAccumulator (acc: Accumulator) (b: PolygonalType * int list) =
    let types = [ yield! acc.Types; fst b ]
    let candidates = snd b

    let values =
        acc.Values
        |> List.map (fun x -> leftJoin x candidates)
        |> List.filter (fun x -> x.Length = types.Length)

    { Types = types; Values = values }

let aggregate (combinations: (PolygonalType * int list) list) =
    let acc = createAccumulator combinations[0] combinations[1]

    let rec loop combination acc =
        match combination with
        | [] -> acc.Values
        | head :: tail ->
            let acc = addToAccumulator acc head
            loop tail acc

    loop combinations[2..] acc

let result =
    numbersOfInterest
    |> permute
    |> Seq.map aggregate
    |> Seq.find (fun x -> x.Length > 0)
    |> Seq.collect id
    |> Seq.sum
