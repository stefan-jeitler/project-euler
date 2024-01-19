#load "../../Utils.fsx"
open Utils

// 1. Split numbers into two sets
let inner = [ 1..5 ]
let outer = [ 6..10 ]

type GroupOfThree =
    { Outer: int
      Between: int
      Inner: int
      Total: int }

// 2. Create pairs that form cycles
let innerCombinations =
    (inner, inner) ||> List.allPairs |> List.filter (fun (a, b) -> a <> b)

let innerCycles =
    [ for a in innerCombinations do
          for b in innerCombinations do
              for c in innerCombinations do
                  for d in innerCombinations do
                      for e in innerCombinations do
                          if
                              snd a = fst b
                              && snd b = fst c
                              && snd c = fst d
                              && snd d = fst e
                              && snd e = fst a
                          then
                              [ a; b; c; d; e ] ]

// 3. Permute the outer elements expect the first number
// since it's the known first digit
let outerPermutations =
    permute outer[1..] |> Seq.map (fun x -> outer[0] :: x) |> Seq.toList

// 4. Create combinations
let createGroupsOfThree outer =
    innerCycles
    |> List.map (fun x -> (outer, x) ||> List.zip)
    |> List.map (fun x ->
        x
        |> List.map (fun (outer, (between, inner)) ->
            { Outer = outer
              Between = between
              Inner = inner
              Total = outer + between + inner }))

let result =
    outerPermutations
    |> List.map createGroupsOfThree
    |> List.collect id
    // 5. Pick all inner lists that share the same total number, concatenate the groups of three and find the max value
    |> List.filter (fun x -> x |> List.groupBy _.Total |> List.length = 1)
    |> List.map (fun x ->
        x
        |> List.fold (fun acc next -> $"%s{acc}%i{next.Outer}%i{next.Between}%i{next.Inner}") "")
    |> List.max
