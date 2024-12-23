open System
open System.IO

let nextSecret (x : int64) =
    let prune x = x % 16777216L
    let x64 = x * 64L
    let x = prune (x ^^^ x64)

    let xd32 = x / 32L
    let x = prune (x ^^^ xd32)

    let x2048 = x * 2048L
    prune (x ^^^ x2048)

let sample = """1
10
100
2024
"""

let parse (input: string) =
    input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

parse sample

let nth next n seed = 
    let rec f x = 
        seq {
            let x1 = next x
            yield x1
            yield! f x1
        }

    f seed
    |> Seq.skip (n - 1)
    |> Seq.head

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let part1 input = 
    input
    |> parse
    |> Array.sumBy (nth nextSecret 2000)

#time
let result1 = part1 input
#time

let price x = int (x % 10L)

[<Struct>]
type History = History of d0: int * d1: int * d2: int * d3: int

let packedHistory (History(d0, d1, d2, d3)) =
    // Differences are -9 to 9
    // Se we can pack into 5 bits each
    d0 + 9 + ((d1 + 9) <<< 5) + ((d2 + 9) <<< 10) + ((d3 + 9) <<< 15)

let priceHistory seed = 
    let rec prices p0 p1 p2 p3 n3 = 
        seq {
            let n4 = nextSecret n3
            let p4 = price n4
            let history = History(
                p1 - p0, 
                p2 - p1, 
                p3 - p2, 
                p4 - p3)
            yield p4, history
            yield! prices p1 p2 p3 p4 n4
        }

    let n0 = seed
    let n1 = nextSecret n0
    let n2 = nextSecret n1
    let n3 = nextSecret n2
    prices (price n0) (price n1) (price n2) (price n3) n3

let part2 input =
    let seeds = parse input

    let histories = 
        seeds 
        |> Array.Parallel.map (fun seed -> 
            // 2000 - 3 initial prices for history = 1997
            seed 
            |> priceHistory 
            |> Seq.take 1997 
            |> Seq.map (fun (v, k) -> packedHistory k, v)
            |> Seq.distinctBy fst
            |> dict
        )

    let priceChanges = 
        histories
        |> Seq.concat
        |> Seq.map (fun (KeyValue(k, _)) -> k)
        |> Seq.distinct
        |> Array.ofSeq

    printfn $"priceChanges count {priceChanges.Length}"

    priceChanges
    |> Array.Parallel.map (fun change ->
        histories
        |> Array.sumBy (fun history ->
            match history.TryGetValue change with
            | true, x -> int64 x
            | false, _ -> int64 0
        )
    )
    |> Array.max

#time
let result2 = part2 input
#time

printfn $"Part1: {result1} Part2: {result2}"
