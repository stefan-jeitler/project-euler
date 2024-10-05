let isInteger n = (n % 1.) < System.Double.Epsilon

// ad - bc = -1
// a = bc - 1 / d

let c = 3.
let d = 7.

// candidates for b
let denominators = [ 1_000_000..-1..1 ]

let result =
    denominators
    // calculate 'a'
    |> Seq.map (fun b -> ((float b * c) - 1.) / d)
    // the first 'a' that is an integer is the numerator we are looking for
    |> Seq.filter isInteger
    |> Seq.head
    |> int
