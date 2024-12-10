open System
open System.IO
open System.Collections.Generic

let sample = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

let digitVal (c: char) = 
    int c - 48

[<Literal>]
let Trailhead = 0

[<Literal>]
let Trailend = 9

[<Struct>]
type Point =
    {
        R: int
        C: int
    }

let Point (r, c) = { R = r; C = c }

type TrailMap(input: string array) =
    member _.Item with get (r, c) = digitVal (input[r][c])
    member _.Item with get (p:Point) = digitVal (input[p.R][p.C])

    member _.Width = input[0].Length
    member _.Height = input.Length

    
    new(input: string) =
        TrailMap(input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries))

let trailheads (map: TrailMap) =
    let rec f r c = 
        seq {
            match map[r,c] with
            | Trailhead -> yield Point(r,c)
            | _ -> ()

            if c = map.Width - 1 then 
                if r = map.Height - 1 then 
                    ()
                else
                    yield! f (r + 1) 0
            else
                yield! f r (c + 1)
        }
    f 0 0

let up p = 
  { p with R = p.R - 1 }
let down p = 
  { p with R = p.R + 1 }
let left p = 
  { p with C = p.C - 1 }
let right p = 
  { p with C = p.C + 1 }

let inBounds (map: TrailMap) p = 
    p.R >= 0 && p.R < map.Height
    && p.C >= 0 && p.C < map.Width

let (|Ascending|_|) (map: TrailMap, currentHeight, p) =
    if inBounds map p then
        let next = map[p]
        if next = currentHeight + 1 then
            ValueSome next
        else
            ValueNone
    else
        ValueNone

let dirs = [| up; down; left; right |]

let rec trailEnds map currentHeight p =
    seq {
        for dir in dirs do 
            let p1 = dir p
            match (map, currentHeight, p1) with
            | Ascending Trailend -> p1
            | Ascending next ->
                yield! trailEnds map next p1
            | _ -> ()
    }

let trailScore (map: TrailMap) start =
    trailEnds map Trailhead start
    |> Seq.distinct
    |> Seq.length

let part1 (input: string) =
    let map = TrailMap input
    map
    |> trailheads
    |> Seq.sumBy (trailScore map)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

let trailRating (map: TrailMap) start =
    trailEnds map Trailhead start
    |> Seq.length

let part2 (input: string) =
    let map = TrailMap input
    map
    |> trailheads
    |> Seq.sumBy (trailRating map)

#time
let result2 = part2 input
#time