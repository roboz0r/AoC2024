open System
open System.IO

let sample = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

type Equation =
    {
        Result: int64
        Values: int64 array
    }

type Op =
    | Add = 0
    | Mul = 1
    | Concat = 2

let parse (input: string) =
    input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        match s.IndexOf(':') with
        | -1 -> failwith $"Invalid line {s}"
        | i ->
            {
                Result = int64 (s[..i - 1])
                Values = (s[i + 1 ..]).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
            }
    )

let eval (values: int64 array) (ops: int) =
    ((0L, -1), values)
    ||> Array.fold (fun (sum, i) x ->
        match i with
        | -1 -> (x, i + 1)
        | _ ->
            match enum<Op>((ops >>> i) % 2) with
            | Op.Add ->
                (sum + x, i + 1)
            | Op.Mul ->
                (sum * x, i + 1)
            | _ -> failwith "Unreachable"
    )
    |> fst

printfn "%B" (pown 3L 11)

let tryFindSolution (eq: Equation) =
    let maxI = pown 2 (eq.Values.Length - 1)
    let rec f i =
        if i = maxI then None
        elif eq.Result = eval eq.Values i then
            Some i
        else
            f (i + 1)
    f 0

parse sample
|> Array.choose (fun x -> 
    tryFindSolution x |> Option.map (fun y -> x.Result))
|> Array.sum

let part1 input =
    parse input
    |> Array.sumBy (fun x -> 
        match tryFindSolution x with
        | Some _ -> x.Result
        | None -> 0L)


let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
#time
let result1 = part1 input
#time

let concatBase10 x y =
    let rec f i n =
        if n = 0L then i
        else
            f (i + 1) (n / 10L)

    let i = f 0 y
    y + (x * (pown 10L i))

let eval2 (values: int64 array) (ops: int) =
    ((0L, -1), values)
    ||> Array.fold (fun (sum, i) x ->
        match i with
        | -1 -> (x, i + 1)
        | _ ->
            match enum<Op>((ops / (pown 3 i)) % 3) with
            | Op.Add ->
                (sum + x, i + 1)
            | Op.Mul ->
                (sum * x, i + 1)
            | Op.Concat ->
                // (int64 $"{sum}{x}", i + 1)
                (concatBase10 sum x, i + 1)
            | _ -> failwith "Unreachable"
    )
    |> fst

let eval2_earlyExit (target: int64) (values: int64 array) (ops: int) =
    let rec f sum i =
        if i = values.Length - 1 then sum
        else
            let x = values[i + 1]
            let sum = 
                match enum<Op>((ops / (pown 3 i)) % 3) with
                | Op.Add ->
                    sum + x
                | Op.Mul ->
                    sum * x
                | Op.Concat ->
                    concatBase10 sum x
                | _ -> failwith "Unreachable"
            if sum > target then -1L
            else f sum (i + 1)

    f values[0] 0


let tryFindSolution2 (eq: Equation) =
    let maxI = pown 3 (eq.Values.Length - 1)
    let rec f i =
        if i = maxI then None
        elif eq.Result = eval2_earlyExit eq.Result eq.Values i then
            Some i
        else
            f (i + 1)
    f 0

let part2 input =
    parse input
    |> Array.sumBy (fun x -> 
        match tryFindSolution x with
        | Some _ -> x.Result
        | None -> 
            match tryFindSolution2 x with
            | Some _ -> x.Result
            | None -> 0L)

#time
let result2 = part2 input
#time

let concatBase10Log x y = 
    let yDigits = if y = 0L then 1 else int (log10 (float y)) + 1
    x * int64 (pown 10L yDigits) + y

#time 
for i in 0L .. 100_000_000L do
    concatBase10Log i i
#time 
#time 
for i in 0L .. 100_000_000L do
    concatBase10 i i
#time 