let dimensions = [ 1UL..2UL..1001UL]

let sumOfSingleGrid dimension = 
        let maxOfGrid = pown dimension 2
        let maxOfPreviousGrid = (max (pown (dimension - 2UL) 2) 1UL)
        let offset = maxOfPreviousGrid
        let steps = dimension - 1UL

        let bottomRight = offset + steps
        let bottomLeft = bottomRight + steps
        let topLeft = bottomLeft + steps
        let topRight = maxOfGrid

        bottomRight + bottomLeft + topLeft + topRight

let rec sumDiagonals (dimensions: uint64 list) = 
    match dimensions with
    | [] -> 0UL
    | head :: tail when head = 1UL -> 1UL + sumDiagonals tail
    | head :: tail -> (sumOfSingleGrid head) + sumDiagonals tail

let result = sumDiagonals dimensions
