let a = [ 2I..100I ]
let b = [ 2..100 ]

let result = 
    a 
    |> List.collect (fun a -> b |> List.map (fun b -> pown a b))
    |> List.distinct
    |> List.length