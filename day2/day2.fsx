open System
open System.IO

let sample = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""" 
    
let parse (s: string) =
    s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun report -> report.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let (|Increasing|Decreasing|Unordered|) xs =
    let output = Array.sort xs
    if xs = output then Increasing output
    else
        let output = Array.sortDescending output
        if xs = output then Decreasing output
        else Unordered

let maxDiff3 (xs: _ array) = 
    let rec f i prev =
        if i = xs.Length then true
        else
            let current = xs[i]
            let x = abs(prev - current)
            if (x > 0 && x <= 3) then
                f (i + 1) current
            else false
    f 1 xs[0]

sample
|> parse
|> Array.map (fun report -> 
    match report with
    | Increasing report
    | Decreasing report -> 
        if maxDiff3 report then 1
        else 0
    | Unordered -> 0
)
|> Array.sum

let part1 input = 
    input
    |> parse
    |> Array.map (fun report -> 
        match report with
        | Increasing report
        | Decreasing report -> 
            if maxDiff3 report then 1
            else 0
        | Unordered -> 0
    )
    |> Array.sum

let result1 = part1 input

type Mode =
    | Increasing
    | Decreasing

let safeCheck (xs: _ array) = 
    let rec f i prev firstChance mode =
        if i = xs.Length then true
        else
            let current = xs[i]
            let x = current - prev
            match mode, x with
            | Increasing, 1 
            | Increasing, 2 
            | Increasing, 3 
            | Decreasing, -1 
            | Decreasing, -2 
            | Decreasing, -3 ->
                f (i + 1) current firstChance mode
            | _ ->
                if firstChance then            
                    f (i + 1) prev false mode
                else 
                    false

    (f 1 xs[0] true Increasing) 
    || (f 1 xs[0] true Decreasing)
    || (f 2 xs[1] false Increasing) // Treat the first report as bad
    || (f 2 xs[1] false Decreasing)

let part2 input = 
    input
    |> parse
    |> Array.filter safeCheck
    |> Array.length

let result2 = part2 input