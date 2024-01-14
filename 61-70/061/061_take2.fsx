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

let join left right =
    let values =
        [ for l in (snd left) do
              for r in (snd right) do
                  if lastTwoDigitsOf l = firstTwoDigitsOf r then
                      [ l; r ] ]

    { Types = [ fst left; fst right ]
      Values = values }

let leftJoin (left: int list) right =
    let first = left[0]
    let last = left |> List.last

    let requiresCycle = (left.Length + 1) = closeCircleAt

    let isCyclicToFirst last =
        lastTwoDigitsOf last = firstTwoDigitsOf first

    let next =
        right
        |> List.tryFind (fun x ->
            lastTwoDigitsOf last = firstTwoDigitsOf x
            && (not requiresCycle || isCyclicToFirst x))

    match next with
    | Some n -> [ yield! left; n ]
    | None -> left

let add (acc: Accumulator) (b: PolygonalType * int list) =
    let types = [ yield! acc.Types; fst b ]
    let candidates = snd b

    let values =
        acc.Values
        |> List.map (fun x -> leftJoin x candidates)
        |> List.filter (fun x -> x.Length = types.Length)

    { Types = types; Values = values }

let aggregate (combinations: (PolygonalType * int list) list) =
    let acc = join combinations[0] combinations[1]

    let rec loop combination acc =
        match combination with
        | [] -> acc.Values
        | head :: tail ->
            let acc = add acc head
            loop tail acc

    loop combinations[2..] acc

let result =
    numbersOfInterest
    |> permute
    |> Seq.map aggregate
    |> Seq.find (fun x -> x.Length > 0)
    |> Seq.collect id
    |> Seq.sum
