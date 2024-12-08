open System
open System.IO

let sample = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""


[<Literal>]
let Empty = '.'

[<Struct>]
type Point =
    {
        R: int
        C: int
    }

let Point (r, c) = { R = r; C = c }

type AntennaMap(input: string array) =
    member _.Item with get (r, c) = input[r][c]
    member _.Item with get (p:Point) = input[p.R][p.C]
    member _.Item with get (r) = input[r]

    member _.Width = input[0].Length
    member _.Height = input.Length

    member this.IsOnMap(p) = p.R >= 0 && p.C >= 0 &&  p.R < this.Height && p.C < this.Width

    new(input: string) =
        AntennaMap(input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries))


let allAntennas (map:AntennaMap) =
    let rec f out r c =
        let out = 
            match map[r,c] with
            | Empty -> out
            | freq -> 
                match out |> Map.tryFind freq with
                | Some others -> 
                    out
                    |> Map.add freq (Point(r, c) :: others)
                | None -> 
                    out |> Map.add freq [Point(r, c)]

        
        if c = map.Width - 1 then 
            if r = map.Height - 1 then 
                out
            else
                f out (r + 1) 0
        else
            f out r (c + 1)

    f Map.empty 0 0

let rec distinctPairs xs =
    seq {
        match xs with
        | head :: tail ->
            for t in tail do
                struct(head, t)
            yield! distinctPairs tail
        | [] -> ()
    }

let antiNodes (map: AntennaMap) p1 p2 =
    let h = abs (p1.R - p2.R)
    let w = abs (p1.C - p2.C)
    let a1 =
        if p1.R < p2.R then 
            if p1.C < p2.C then
                Point(p1.R - h, p1.C - w)
            else
                Point(p1.R - h, p1.C + w)
        else
            if p1.C < p2.C then 
                Point(p1.R + h, p1.C - w)
            else
                Point(p1.R + h, p1.C + w)

    let a2 =
        if p2.R < p1.R then 
            if p2.C < p1.C then
                Point(p2.R - h, p2.C - w)
            else
                Point(p2.R - h, p2.C + w)
        else
            if p2.C < p1.C then 
                Point(p2.R + h, p2.C - w)
            else
                Point(p2.R + h, p2.C + w)

    // printfn $"{a1}, {a2}"
    [|
        if map.IsOnMap(a1) then 
            a1
        if map.IsOnMap(a2) then
            a2
    |]

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    
let part1 (input: string) =
    input
    |> AntennaMap 
    |> fun map -> 
        map
        |> allAntennas
        |> Map.map (fun k v ->
            v 
            |> distinctPairs
            |> Seq.collect (fun struct(p1, p2) -> antiNodes map p1 p2)
        )
        |> Seq.collect (fun (KeyValue(k, v)) -> v)
        |> Set
        |> _.Count


#time
let result1 = part1 input
#time

let antiNodes2 (map: AntennaMap) p1 p2 =
    let fall = p2.R - p1.R
    let span = p2.C - p1.C

    [|
        for i in -map.Height .. map.Height do
            let a = Point(p2.R + (fall * i), p2.C + (span * i))
            if map.IsOnMap a then
                a
    |]

let part2 (input: string) =
    let map = AntennaMap input
    
    map
    |> allAntennas
    |> Map.map (fun k v ->
        v 
        |> distinctPairs
        |> Seq.collect (fun struct(p1, p2) -> antiNodes2 map p1 p2)
    )
    |> Seq.collect (fun (KeyValue(k, v)) -> v)
    |> Set
    |> _.Count


#time
let result2 = part2 input
#time