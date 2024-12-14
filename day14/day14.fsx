#r "nuget: FParsec"
open System
open System.IO

[<Struct>]
type Point<[<Measure>] 'M> =
    {
        X: int64<'M>
        Y: int64<'M>
    }

[<Measure>]
type tile

[<Measure>]
type s

[<Struct>]
type Robot =
    {
        Position: Point<tile>
        Velocity: Point<tile/s>
    }
// position of p=0,0 means the robot is all the way in the top-left corner

let sample = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

let inline int64m<[<Measure>] 'M> x = LanguagePrimitives.Int64WithMeasure<'M> x

module Scenario =
    open FParsec
    let pPoint<[<Measure>] 'M> = pipe3 pint64 (pchar ',') pint64 (fun x _ y -> { X = int64m<'M> x; Y = int64m<'M> y })
    let pRobot = pipe5 (pstring "p=") pPoint (pstring " v=") pPoint newline (fun _ p _ v _ -> { Position = p; Velocity = v })
    let pScenario = many pRobot .>> eof

    let parse (input: string) =
        match run pScenario input with
        | Success (s, _, _) -> s
        | Failure (s, err, _) ->
            failwithf "%s %A" s err

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

input |> Scenario.parse

let move w h robot = 
    let { X = px; Y = py } = robot.Position
    let { X = vx; Y = vy } = robot.Velocity

    let px1 = 
        let px1 = (px + 1L<s> * vx) % w
        if px1 < 0L<_> then px1 + w else px1

    let py1 = 
        let py1 = (py + 1L<s> * vy) % h
        if py1 < 0L<_> then py1 + h else py1

    { robot with
        Position = { X = px1; Y = py1 }
    }

type Quadrant = 
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | Midline

let quadrant (w: int64<tile>) (h: int64<tile>) robot =
    let { X = px; Y = py } = robot.Position
    let w2 = w/2L
    let h2 = h/2L
    if px < w2 && py < h2 then 
        TopLeft
    elif px > w2 && py < h2 then 
        TopRight
    elif px < w2 && py > h2 then 
        BottomLeft
    elif px > w2 && py > h2 then 
        BottomRight
    else Midline

let step width height (positions: Robot array) =
    for i in 0 .. (positions.Length - 1) do
        positions[i] <- move width height positions[i]

let stepN steps width height (positions: Robot array) =
    for s in 1 .. steps do
        step width height positions
    positions

let safetyFactor width height positions =
    positions
    |> Array.countBy (quadrant width height)
    |> Array.choose (fun (q, count) -> if q <> Midline then Some (int64 count) else None)
    |> Array.reduce (fun c1 c2 -> c1 * c2)

let render (width: int64<tile>) (height: int64<tile>) positions =
    let map = 
        positions 
        |> Array.countBy _.Position
        |> Map

    for h in 0L .. int64 (height - 1L<tile>) do
        for w in 0L .. int64 (width - 1L<tile>) do
            let c = 
                map 
                |> Map.tryFind { X = int64m w; Y = int64m h } 
                |> Option.map (fun count -> char (count + 48)) 
                |> Option.defaultValue '.'
            Console.Write(c)
        Console.WriteLine("|")

let part1 width height input =
    input
    |> Scenario.parse
    |> Array.ofList
    |> stepN 100 width height
    |> safetyFactor width height

#time
// let result1 = part1 11L<_> 7L<_> sample
let result1 = part1 101L<_> 103L<_> input
#time

let runInteractive width height input =
    let scenario = Scenario.parse input
    let positions = Array.ofList scenario
    let mutable count = 0
    let mutable key = ' '

    printfn "Step %i" count
    render width height positions
    stepN 12 width height positions |> ignore
    count <- count + 12
    while key = ' ' do
        printfn "Step %i" count
        render width height positions
        stepN 103 width height positions |> ignore
        count <- count + 103
        let read = Console.ReadKey()
        key <- read.KeyChar

    count


// runInteractive 101L<_> 103L<_> input
// Found pattern at 12, 115, 218 so periodic with 12 + n * height

let result2 = 8149

let part2 steps width height input =
    let scenario = Scenario.parse input
    let positions = Array.ofList scenario
    stepN steps width height positions |> ignore
    render width height positions

#time
part2 result2 101L<_> 103L<_> input
#time
