open System

let start = DateOnly.Parse("1901-01-01")
let ``end`` = DateOnly.Parse("2000-12-31")

let rec months (next: DateOnly) =
    seq {
        if next >= ``end`` then
            yield next
        else
            yield next
            yield! months (next.AddMonths(1))
    }

let result =
    months start
    |> Seq.filter (fun x -> x.DayOfWeek = DayOfWeek.Sunday)
    |> Seq.length
