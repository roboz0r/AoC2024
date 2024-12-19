open System
open System.IO
open System.Collections.Generic

[<Struct>]
type Towel = Towel of string

[<Struct>]
type Design = 
    | Design of string
    member this.Length = let (Design d) = this in d.Length

[<Struct>]
type Arrangement = 
    | Arrangement of Towel[]
    override this.ToString (): string = 
        let (Arrangement a) = this
        String.Join(",", a)

let sample = """r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"""

let parse (input: string) = 
    let arr = input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    let towels = arr[0] |> _.Split(", ") |> Array.map Towel
    let designs = 
        arr[1..]
        |> Array.map Design
    towels, designs

let fitsDesign (Design design) start (Towel t) = 
    if start + t.Length > design.Length then ValueNone
    elif design.AsSpan(start, t.Length).SequenceEqual(t) then 
        ValueSome (struct (Towel t, start + t.Length))
    else
        ValueNone

let nextTowels design start (towels: Towel[]) = 
    seq {
        for t in towels do
            match fitsDesign design start t with
            | ValueSome x -> x
            | ValueNone -> ()
    }

let findArrangements design towels =
    let rec f (Arrangement arr) (known: Dictionary<int, Arrangement>) start = 
        seq {
            for (t, start) in nextTowels design start towels do
                let arrNext = Arrangement (Array.append arr [|t|])
                if start = design.Length then arrNext
                else
                    if known.ContainsKey start then ()
                    else
                        known[start] <- arrNext
                        // printfn $"%03i{start} %O{arrNext}"
                        yield! f arrNext known start
        }

    f (Arrangement [||]) (Dictionary()) 0

let part1 input = 
    let (towels, designs) = parse input
    let towels = towels |> Array.sortByDescending (fun (Towel t) -> t.Length)
    designs
    |> Array.filter (fun design ->
        // printf $"{design}..."
        if 
            findArrangements design towels
            |> Seq.isEmpty
            |> not then
            // printfn $"OK"
            true
        else
            // printfn $"FAIL"
            false
    )
    |> Array.length


let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

type CacheKey = { Remaining: string }

let key start (Design design) = { Remaining = design[start..] }

let countPossibilities towels (cache: Dictionary<CacheKey, int64>) (design: Design) =
    let rec f start = 
        if start = design.Length then 1L
        else
            let key = key start design

            match cache.TryGetValue(key) with
            | true, count -> count
            | false, _ ->
                let count =
                    towels
                    |> Array.sumBy (fun t ->
                        match fitsDesign design start t with
                        | ValueSome (_, next) -> f next
                        | ValueNone -> 0L
                    )
                cache[key] <- count
                count

    f 0

let part2 input =
    let (towels, designs) = parse input
    let arr = designs |> Array.map (countPossibilities towels (Dictionary()))
    arr |> Array.sumBy (fun x -> if x > 0L then 1L else 0L), arr |> Array.sum

#time
let (result1_2, result2) = part2 input
#time
printfn $"Part1: {result1} or {result1_2} Part2: {result2}"
