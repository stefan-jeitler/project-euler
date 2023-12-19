open System.IO

let filePath = Path.Combine(__SOURCE_DIRECTORY__, "triangle.txt")

let triangle =
    File.ReadAllLines filePath
    |> Array.map (fun x -> x.Trim().Split(" ") |> Array.map int |> Array.toList)
    |> Array.toList

let mergeLines (a: int list) (b: int list) =
    if abs (a.Length - b.Length) <> 1 then
        failwith "length mismatch between adjacent lines"

    let (top, bottom) = if a.Length > b.Length then (b, a) else (a, b)

    let maxOfAdjacentBottom indexOfTop =
        max (bottom[indexOfTop]) bottom[indexOfTop + 1]

    top |> List.mapi (fun i x -> x + maxOfAdjacentBottom i)

let folder (next: int list) (acc: int list) =
    match acc with
    | [] -> next
    | _ -> mergeLines next acc

let result = (triangle, []) ||> List.foldBack folder |> List.head
