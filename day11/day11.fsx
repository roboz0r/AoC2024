open System
open System.IO

[<Struct>]
type Stone = Stone of int64

// Rules
// Stone 0 -> Stone 1
// AB = even digits Stone AB -> Stone A * Stone B (1000 -> 10 * 0)
// Else Stone x -> Stone (x * 2024)

let (|ZeroRule|_|) (Stone x) =
    if x = 0L then
        ValueSome (Stone 1L)
    else
        ValueNone

let digitCount (x: int64) =
    let rec f x i = 
        if x = 0L then i
        else
            f (x / 10L) (i + 1)

    f (x / 10L) 1

let (|EvenDigitsRule|_|) (Stone x) =
    let digits = digitCount x
    match digits % 2 with
    | 0 -> 
        let split = pown 10L (digits / 2)
        ValueSome([| Stone (x / split); Stone (x % split)|])
    | _ -> ValueNone

let (|Mul2024|) (Stone x) =
    Stone (x * 2024L)

let next stone = 
    match stone with
    | ZeroRule s -> [| s |]
    | EvenDigitsRule ss -> ss
    | Mul2024 s -> [|s|]


let next25 stone =
    let rec f (stone: Stone) i =
        seq {
            if i = 25 then stone
            else
                let stones = next stone
                for s in stones do
                    yield! f s (i + 1)
        }
    f stone 0

let part1 (input: string) =
    input.Split([|' '; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (int64 >> Stone)
    |> Array.sumBy (next25 >> Seq.length)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

open System.Collections.Generic

let cachable25 stone =
    next25 stone
    |> Array.ofSeq
    |> Array.countBy id

let cached25 (dict: Dictionary<int64, _>) stone =
    let (Stone s) = stone
    match dict.TryGetValue(s) with
    | true, xs -> xs
    | false, _ -> 
        let xs = cachable25 stone
        dict.Add(s, xs)
        xs

let part2 (input: string) =
    let stones = 
        input.Split([|' '; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (int64 >> Stone)

    let cache = Dictionary()
    let cached25 stone = cached25 cache stone

    let rec f i stone =
        seq {
            let stones = cached25 stone
            for (nextStone, count) in stones do
                let count = int64 count
                match i with
                | 2 -> count
                | _ ->
                    f (i + 1) nextStone
                    |> Seq.sumBy (fun x -> x * count)
        }

    stones
    |> Array.sumBy (f 0 >> Seq.sum)

    
#time
let result2 = part2 input
#time

type CacheKey =
    {
        Stone: int64
        Remaining: int
    }

let key (Stone stone) rem = 
    {
        Stone = stone
        Remaining = rem
    }

let countStonesAfterBlinks blinks initialStones =
    let stoneCounts = Dictionary<CacheKey, int64>()

    let rec calculateStoneCount stone (remaining: int) =
        match remaining with
        | 0 -> 1L
        | _ ->
            let key = key stone remaining
            match stoneCounts.TryGetValue key with
            | true, count -> count
            | _ ->
                let count =
                    match next stone with
                    | [| s |] -> calculateStoneCount s (remaining - 1)
                    | [| s1; s2 |] -> 
                        calculateStoneCount s1 (remaining - 1)
                        + calculateStoneCount s2 (remaining - 1)
                    | _ -> failwith "Unreachable"

                stoneCounts.[key] <- count
                count

    initialStones |> Array.sumBy (fun x -> calculateStoneCount x blinks)


let part2_alt (input: string) =
    let stones = 
        input.Split([|' '; '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (int64 >> Stone)

    countStonesAfterBlinks 75 stones

    
#time
let result2_alt = part2_alt input
#time

