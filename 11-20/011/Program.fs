open System
open System.IO

let (/>) a b = Path.Combine(a, b)

let cwd = AppContext.BaseDirectory
let filePath = cwd /> "grid.txt"

type Position = { Line: int; Column: int }

module Position =
    let initialPosition = { Line = 0; Column = 0 }

    let decrementColumn pos = { pos with Column = pos.Column - 1 }
    let incrementColumn (pos: Position) = { pos with Column = pos.Column + 1 }

    let resetColumn pos = { pos with Column = 0 }

    let decrementLine pos = { pos with Line = pos.Line - 1 }
    let incrementLine pos = { pos with Line = pos.Line + 1 }

type Grid = string[,]

module Grid =
    let value (grid: Grid) (pos: Position) : string = grid[pos.Line, pos.Column]

    let moveNext (grid: Grid) (pos: Position) : Position option =

        let line = pos.Line
        let lineLength = grid.GetLength(0)

        let column = pos.Column
        let columnLength = grid.GetLength(1)

        match (line, lineLength), (column, columnLength) with
        | (l, lLength), _ when l < 0 || l >= lLength -> None
        | _, (c, cLength) when c < 0 || c >= cLength -> None
        | _, (c, cLength) when (c + 1) < cLength -> Some(Position.incrementColumn pos)
        | (l, lLength), (c, cLength) when (l + 1) < lLength && (c < cLength) ->
            Some((Position.incrementLine >> Position.resetColumn) pos)
        | _ -> None

    let exists (grid: Grid) (pos: Position) =
        match moveNext grid pos with
        | Some _ -> true
        | None -> false

let adjacentPositions (grid: Grid) (startPos: Position) =
    let existsInGrid = Grid.exists grid

    let getPositions maxDepth moveInDirection =
        let rec getPositions' pos (acc: Position list) =
            if acc.Length < maxDepth && existsInGrid pos then
                getPositions' (moveInDirection pos) (pos :: acc)
            else
                acc

        getPositions' startPos []

    let towardsRight = Position.incrementColumn
    let towardsDownRight = (Position.incrementLine >> Position.incrementColumn)
    let downwards = Position.incrementLine
    let towardsDownLeft = (Position.incrementLine >> Position.decrementColumn)

    let getNextFourPositions = getPositions 4

    [ getNextFourPositions towardsRight
      getNextFourPositions towardsDownRight
      getNextFourPositions downwards
      getNextFourPositions towardsDownLeft ]
    |> List.filter (fun x -> x.Length = 4)

let parseLine (l: string) = l.Split(' ')
let grid = File.ReadAllLines filePath |> Array.map parseLine |> array2D

let productsOfFourAdjacentItems grid startPos (acc: int list) =
    let adjacentPositionsInGrid = adjacentPositions grid
    let gridValue = Grid.value grid
    let moveNext = Grid.moveNext grid

    let rec productsOfFourAdjacentItems' pos acc =
        match pos with
        | None -> acc
        | Some p ->
            let productsOfAdjacentItems =
                adjacentPositionsInGrid p
                |> List.map (fun x -> x |> List.map gridValue |> List.map int |> List.fold (*) 1)

            let largestProductOfAdjacentItems =
                match productsOfAdjacentItems.Length with
                | 0 -> 0
                | _ -> productsOfAdjacentItems |> List.max

            let nextPos = moveNext p
            productsOfFourAdjacentItems' nextPos (largestProductOfAdjacentItems :: acc)

    productsOfFourAdjacentItems' startPos acc

let maxProduct =
    productsOfFourAdjacentItems grid (Some Position.initialPosition) [] |> List.max

printfn $"result %i{maxProduct}"
