open System.IO

let filePath = Path.Combine(__SOURCE_DIRECTORY__, "triangle.txt")

let triangle =
    File.ReadAllLines filePath
    |> Array.map (fun x -> x.Trim().Split(" ") |> Array.map int |> Array.toList)
    |> Array.rev

let rec findPath line (acc: int list) =
    let isFirstLine = line = 0
    let isLastLine = triangle.Length - 1 = line
    let skipLine () = findPath (line + 1) acc

    let maxAdjacent currentIdx (line: int list) =
        max (line[currentIdx]) (line[currentIdx + 1])

    let sumEntry number index =
        if line < 2 then
            number + (maxAdjacent index triangle[line - 1])
        else
            number + maxAdjacent index acc

    if isFirstLine then
        skipLine ()
    elif isLastLine then
        sumEntry (triangle[line][0]) 0
    else
        let folder (idx, acc) current =
            (idx + 1, (sumEntry current idx) :: acc)

        let _, acc = triangle[line] |> List.fold folder (0, [])

        findPath (line + 1) (acc |> List.rev)

let result = findPath 0 []
