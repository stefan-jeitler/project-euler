let init: Map<string, uint64 list> = Map.empty
let toSortedString: uint64 -> string = string >> Seq.sort >> System.String.Concat

let add (map: Map<string, uint64 list>) key value =
    map.Change(
        key,
        function
        | Some l -> Some(value :: l)
        | None -> Some [ value ]
    )

let testNumber (acc: Map<string, uint64 list>) cube =
    let key = toSortedString cube
    let acc = add acc key cube

    if acc[key].Length = 5 then
        acc, Some (acc[key] |> List.min)
    else 
        acc, None

let cubes =
    [ 5000UL .. 9_000UL ]
    |> List.map (fun b -> pown b 3)

let findLowestOfFivePermutedCubes (cubes: uint64 list) =
    let rec loop c acc =
        match c with
        | [] -> None
        | head :: tail ->
            let (acc, result) = testNumber acc head
            match result with
            | Some r -> Some r
            | None -> loop tail acc

    loop cubes init

let result = 
    findLowestOfFivePermutedCubes cubes
    |> Option.get
