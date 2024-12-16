open System
open System.IO
open System.Collections.Generic

[<Literal>]
let Empty = '.'

[<Literal>]
let Wall = '#'

[<Literal>]
let Start = 'S'

[<Literal>]
let End = 'E'

let sample1 = """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"""

let sample2 = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"""

[<Struct>]
type Point = 
    {
        R: int
        C: int
    }
    
let up p = 
  { p with R = p.R - 1 }
let down p = 
  { p with R = p.R + 1 }
let left p = 
  { p with C = p.C - 1 }
let right p = 
  { p with C = p.C + 1 }

type Direction =
    | North
    | East
    | South
    | West

type Maze(input: string array) =
    let width() = input[0].Length
    let height() = input.Length

    let rec findPoint f r c =
        if f (input[r][c]) then 
            { R = r; C = c }
        elif (c + 1) < width() then 
            findPoint f r (c + 1)
        elif (r + 1) < height() then
            findPoint f (r + 1) 0
        else
            failwith "Not found"

    let startP = 
        findPoint ((=) Start) 0 0

    let endP = 
        findPoint ((=) End) 0 0 

    member _.Item with get (r, c) = (input[r][c])
    member _.Item with get (p:Point) = (input[p.R][p.C])

    member _.Width = input[0].Length
    member _.Height = input.Length

    member this.Start = startP

    member this.End = endP

    new(input: string) =
        Maze(input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries))

type Move =
    | Forward
    | Clockwise
    | Anticlockwise

module Move = 
    let score =
        function
        | Forward -> 1L
        | Clockwise
        | Anticlockwise -> 1000L

    let clockwise = 
        function
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    let antiClockwise = 
        function
        | North -> West
        | East -> North
        | South -> East
        | West -> South

type Location =
    {
        Position: Point
        Direction: Direction
    }

type Path =
    {
        Score: int64
        Moves: Move list
        Position: Point
        Direction: Direction
        History: Set<Location>
    }
    member this.Location = { Position = this.Position; Direction = this.Direction }

type Paths(start) = 
    let mutable map = Map.ofList [ start.Score, [start] ]
    let mutable shortestPaths = Map.ofList [ start.Location, start ]

    // Take advantage of map head always being the lowest score
    let pop () = 
        let (KeyValue(_, head)) = map |> Seq.head
        match head with
        | [] -> failwith "Paths contained empty list"
        | [first] -> 
            map <- map |> Map.remove first.Score
            first
        | first :: tail ->
            map <- map |> Map.add first.Score tail
            first

    let tryPop () = 
        match map |> Seq.tryHead with
        | Some (KeyValue(_, head)) ->
            match head with
            | [] -> failwith "Paths contained empty list"
            | [first] -> 
                map <- map |> Map.remove first.Score
                Some first
            | first :: tail ->
                map <- map |> Map.add first.Score tail
                Some first
        | None -> None

    let push (path: Path) =
        let maybePath =
            match path.Location |> shortestPaths.TryFind with
            | None -> 
                shortestPaths <- shortestPaths |> Map.add path.Location path
                Some path
            | Some p1 ->
                if p1.Score < path.Score then 
                    None
                else
                    shortestPaths <- shortestPaths |> Map.add path.Location path
                    Some path

        match maybePath with
        | Some path ->
            match map |> Map.tryFind path.Score with
            | Some paths ->
                let newPaths = 
                    path :: paths
                map <- map |> Map.add path.Score newPaths
            | None -> 
                map <- map |> Map.add path.Score [path]
        | None -> ()

    
    member _.Pop() = pop()
    member _.TryPop() = tryPop()
    member _.Push(path) = push path
    member _.Push(paths) = paths |> Array.iter push
    member _.ShortestPaths = shortestPaths
    member _.CurrentPaths = map

let forwardDir (path:Path) =
    match path.Direction with
    | North -> up path.Position
    | East -> right path.Position
    | South -> down path.Position
    | West -> left path.Position

let tryForward (maze: Maze) (path: Path) = 
    let p1 = forwardDir path
    let l = { Direction = path.Direction; Position = p1 }
    match path.History.Contains(l), maze[p1] with
    | true, _
    | _, Wall -> None
    | _ -> 
        Some { 
            path with
                Score = path.Score + Move.score Forward
                Position = p1
                Moves = Forward :: path.Moves
                History = path.History.Add l
        }

let tryClockwise (maze: Maze) (path: Path) = 
    let l = { Direction = Move.clockwise path.Direction; Position = path.Position }
    if path.History.Contains l then None
    else
        let nextPath =
            { path with
                Score = path.Score + Move.score Clockwise
                Direction = l.Direction
                Moves = Clockwise :: path.Moves
                History = path.History.Add l
            }
        let p1 = forwardDir nextPath
        match maze[p1] with
        | Wall -> None
        | _ -> Some nextPath

let tryAnticlockwise (maze: Maze) (path: Path) = 
    let l = { Direction = Move.antiClockwise path.Direction; Position = path.Position }
    if path.History.Contains l then None
    else
        let nextPath =
            { path with
                Score = path.Score + Move.score Anticlockwise
                Direction = l.Direction
                Moves = Anticlockwise :: path.Moves
                History = path.History.Add l
            }
        let p1 = forwardDir nextPath
        match maze[p1] with
        | Wall -> None
        | _ -> Some nextPath

let moves = [| tryForward; tryClockwise; tryAnticlockwise |]

let rec advance (maze: Maze) (path: Path) = 
    let nextMoves =
        moves
        |> Array.choose (fun move -> move maze path)

    match nextMoves with
    | [| next |] -> 
        if maze.End = next.Position then nextMoves
        else
            // If you can only do one thing, just keep following the path
            advance maze next
    | _ -> nextMoves

let findLowestScore (maze: Maze) = 
    let rec f (maze: Maze) (allPaths: Paths) =
        let head = allPaths.Pop()
        // printfn $"{head.Score} {head.Direction} {head.Position.R}, {head.Position.C}"
        if maze.End = head.Position then head
        else
            let newPaths = advance maze head
            allPaths.Push newPaths
            f maze allPaths

    let start = 
        {
            Score = 0L
            Position = maze.Start
            Direction = East
            Moves = []
            History = Set.singleton { Direction = East; Position = maze.Start }
        }

    let allPaths = Paths(start)
    f maze allPaths

let part1 (input: string) =
    input
    |> Maze
    |> findLowestScore
    |> _.Score

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

let findPointsOnBestPaths (maze: Maze) = 
    let rec f (maze: Maze) (allPaths: Paths) pointsOnBestPaths bestScore =
        match allPaths.TryPop() with
        | Some head ->
            // printfn $"{head.Score} {head.Direction} {head.Position.R}, {head.Position.C}"
            if maze.End = head.Position then
                if bestScore = -1L then
                    // printfn $"Found best score {head.Score}"
                    let bestScore = head.Score
                    let pointsOnBestPaths = head.History
                    f maze allPaths pointsOnBestPaths bestScore
                elif bestScore = head.Score then
                    // printfn $"Found alt path {head.Score}"
                    let pointsOnBestPaths = Set.union pointsOnBestPaths head.History
                    f maze allPaths pointsOnBestPaths bestScore
                elif bestScore < head.Score then
                    // printfn $"Excluded {head.Score}"
                    f maze allPaths pointsOnBestPaths bestScore
                else
                    failwith $"Found better score {bestScore} {head}"

            else
                let newPaths = advance maze head
                allPaths.Push newPaths
                f maze allPaths pointsOnBestPaths bestScore
        | None -> pointsOnBestPaths

    let start = 
        {
            Score = 0L
            Position = maze.Start
            Direction = East
            Moves = []
            History = Set.singleton { Direction = East; Position = maze.Start }
        }

    let allPaths = Paths(start)
    f maze allPaths Set.empty -1L

let render (maze: Maze) (points: Set<Point>) = 
    for r in 0 .. maze.Height - 1 do
        for c in 0 .. maze.Width - 1 do
            if points.Contains({ R = r; C = c }) then 
                Console.Write('O')
            else
                Console.Write(maze[r,c])
        Console.WriteLine()
    points

let part2 (input: string) =
    let maze = Maze input

    maze
    |> findPointsOnBestPaths
    |> Set.map _.Position
    // |> (render maze)
    |> _.Count
    
#time
let result2 = part2 input
#time
printfn $"Part1: {result1} Part2: {result2}"
