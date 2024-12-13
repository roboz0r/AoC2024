open System
open System.IO

[<Struct>]
type Point =
    {
        R: int
        C: int
    }

let Point (r, c) = { R = r; C = c }

type Plant = char

[<Struct>]
type LocatedPlant =
    {
        Plant: Plant
        Point: Point
    }

type Region =
    {
        Plant: Plant
        Plants: Set<Point>
        Area: int64
        Perimeter: int64
    }


type Plot(input: string array) =
    member _.Item with get (r, c) = (input[r][c])
    member _.Item with get (p:Point) = (input[p.R][p.C])

    member _.Width = input[0].Length
    member _.Height = input.Length

    
    new(input: string) =
        Plot(input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries))

let up p = 
  { p with R = p.R - 1 }
let down p = 
  { p with R = p.R + 1 }
let left p = 
  { p with C = p.C - 1 }
let right p = 
  { p with C = p.C + 1 }

let inBounds (map: Plot) p = 
    p.R >= 0 && p.R < map.Height
    && p.C >= 0 && p.C < map.Width

let dirs = [| up; down; left; right |]

let startRegion (map: Plot) p =
    let plant = map[p]
    {
        Plant = plant
        Plants = Set.singleton p
        Area = 1L
        Perimeter = 0L
    }

let mapRegion map plant p =
    let rec f (map: Plot) plant visited area perimeter (p: Point) =
        let inRegion = 
            dirs
            |> Array.map (fun dir -> dir p)
            |> Array.filter (fun p -> inBounds map p && map[p] = plant)

        let perimeter = perimeter + int64 (dirs.Length - inRegion.Length)
        let area = area + 1L
        let (visited, area, perimeter) =
            ((visited, area, perimeter), inRegion)
            ||> Array.fold (fun (visited, area, perimeter) p ->
                if visited |> Set.contains p then 
                    (visited, area, perimeter)
                else
                    let visited = visited |> Set.add p
                    f map plant visited area perimeter p

            )

        (visited, area, perimeter)

    let (visited, area, perimeter) = f map plant (Set.singleton p) 0L 0L p
    {
        Plant = plant
        Plants = visited
        Area = area
        Perimeter = perimeter
    }

let mapRegions (map: Plot) =
    let rec f (map: Plot) allRegions visited r c =
        let p = Point(r, c)
        let (allRegions, visited) =
            if not(visited |> Set.contains p) then
                let plant = map[p]
                let region = mapRegion map plant p
                allRegions |> Map.add {Plant = region.Plant; Point = region.Plants |> Seq.head } region
                , Set.union visited region.Plants
            else
                (allRegions, visited)

        if c = map.Width - 1 then 
            if r = map.Height - 1 then 
                allRegions
            else
                f map allRegions visited (r + 1) 0
        else
            f map allRegions visited r (c + 1)

    f map Map.empty Set.empty 0 0

let sample1 = """AAAA
BBCD
BBCC
EEEC"""

let sample2 = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"""

let sample3 = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

let plot1 = Plot sample1
let plot2 = Plot sample2
let plot3 = Plot sample3

let part1 (input: string) =
    input
    |> Plot
    |> mapRegions
    |> Seq.sumBy (fun (KeyValue(id, region)) ->
        region.Area * region.Perimeter)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

let groupContiguous by (ps: Point array) =
    let ps = ps |> Array.sortBy by
    let rec f acc1 acc2 lastP i =
        if i = ps.Length then 
            acc2 :: acc1
        else
            let p = ps[i]
            if by p = by lastP + 1 then 
                f acc1 (p :: acc2) p (i + 1)
            else
                f (acc2 :: acc1) [p] p (i + 1)

    
    if ps.Length = 0 then []
    else
        let p = ps[0]
        f [] [p] p 1

let groupCs ps = groupContiguous _.C ps
let groupRs ps = groupContiguous _.R ps

let countSides (map: Plot) (region: Region) =
    let plant = region.Plant
    let verticals =
        region.Plants
        |> Seq.groupBy _.C
        |> Seq.map (fun (c, points) -> (c, points |> Array.ofSeq |> Array.sort))
        |> Array.ofSeq

    let horizontals =
        region.Plants
        |> Seq.groupBy _.R
        |> Seq.map (fun (c, points) -> (c, points |> Array.ofSeq |> Array.sort))
        |> Array.ofSeq

    let leftFences =
        verticals
        |> Array.collect (fun (c, points) ->
            points
            |> Array.filter (fun p -> 
                let pLeft = left p
                p.C = 0 || (inBounds map pLeft && plant <> map[pLeft]))
            |> groupRs
            |> List.map _.Length
            |> Array.ofList
        )

    let rightFences =
        verticals
        |> Array.collect (fun (c, points) ->
            points
            |> Array.filter (fun p -> 
                let pRight = right p
                p.C = map.Width - 1 || (inBounds map pRight && plant <> map[pRight]))
            |> groupRs
            |> List.map _.Length
            |> Array.ofList
        )

    let topFences =
        horizontals
        |> Array.collect (fun (c, points) ->
            points
            |> Array.filter (fun p -> 
                let pUp = up p
                p.R = 0 || (inBounds map pUp && plant <> map[pUp]))
            |> groupCs
            |> List.map _.Length
            |> Array.ofList
        )

    let bottomFences =
        horizontals
        |> Array.collect (fun (c, points) ->
            points
            |> Array.filter (fun p -> 
                let pDown = down p
                p.R = map.Height - 1 || (inBounds map pDown && plant <> map[pDown]))
            |> groupCs
            |> List.map _.Length
            |> Array.ofList
        )

    [|
        leftFences
        rightFences
        topFences
        bottomFences
    |]
    |> Array.sumBy _.Length
    |> int64


let part2 (input: string) =
    let plot = Plot input
    
    plot
    |> mapRegions
    |> Seq.sumBy (fun (KeyValue(k, v)) ->
        printfn $"{k}"
        let sides = countSides plot v
        sides * v.Area
    )

#time
let result2 = part2 input
#time
