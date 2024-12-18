open System
open System.IO
open System.Text

[<Literal>]
let Empty = '.'

[<Literal>]
let Corrupted = '#'

[<Literal>]
let Wall = '#'

[<Literal>]
let Start = 'S'

[<Literal>]
let End = 'E'

[<Struct>]
type Point = 
    {
        X: int
        Y: int
    }

let sample = """5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"""

let sampleGrid = 6
let inputGrid = 70

type Input = Input of Point[]

let parse (input: string) = 
    input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        match s.IndexOf(',') with
        | -1 -> failwith $"Unexpected input {s}"
        | i -> { X = Int32.Parse(s.AsSpan(0, i)); Y = Int32.Parse(s.AsSpan(i + 1))}
    )
    |> Input

type MemorySpace(grid: char[,]) =

    member this.Item 
        with get (x, y) = grid[x, y]
        and set (x, y) v = grid[x, y] <- v

    member this.Item 
        with get (p:Point) = grid[p.X,p.Y]
        and set (p:Point) v = grid[p.X,p.Y] <- v

    member this.Width = Array2D.length1 grid
    member this.Height = Array2D.length2 grid

    member this.DropBytes((Input points), count) = 
        for i in 0 .. count  - 1 do
            this[points[i]] <- Corrupted

    member this.DropStartEnd(startP, endP) = 
        this[startP] <- Start
        this[endP] <- End

    new (maxXY: int) = 
        let len = maxXY + 1
        MemorySpace(Array2D.create len len Empty)

let render = 
    let renderBuffer = StringBuilder()
    fun (working: MemorySpace) ->
        renderBuffer.Clear() |> ignore
        renderBuffer.Append(Wall, working.Width + 2).AppendLine() |> ignore
        for y in 0 .. (working.Height - 1) do
            renderBuffer.Append(Wall) |> ignore
            for x in 0 .. (working.Width - 1) do
                renderBuffer.Append(working[x,y]) |> ignore
            renderBuffer.Append(Wall).AppendLine() |> ignore

        renderBuffer.Append(Wall, working.Width + 2).AppendLine() |> ignore
        renderBuffer.ToString()


    
let up p = 
  { p with Y = p.Y - 1 }
let down p = 
  { p with Y = p.Y + 1 }
let left p = 
  { p with X = p.X - 1 }
let right p = 
  { p with X = p.X + 1 }

type Direction =
    | Up
    | Right
    | Down
    | Left


type Maze(input: string array) =
    let width() = input[0].Length
    let height() = input.Length

    let rec findPoint f y x =
        if f (input[y][x]) then 
            { Y = y; X = x }
        elif (x + 1) < width() then 
            findPoint f y (x + 1)
        elif (y + 1) < height() then
            findPoint f (y + 1) 0
        else
            failwith "Not found"

    let startP = 
        findPoint ((=) Start) 0 0

    let endP = 
        findPoint ((=) End) 0 0 

    member _.Item with get (x, y) = (input[y][x])
    member _.Item with get (p:Point) = (input[p.Y][p.X])

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
        | Forward -> 1000L
        | Clockwise
        | Anticlockwise -> 1L

    let clockwise = 
        function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let antiClockwise = 
        function
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

type Location =
    {
        Position: Point
        Direction: Direction
    }

type Path =
    {
        Score: int64
        // Moves: Move list
        Position: Point
        Direction: Direction
        History: Set<Location>
    }
    member this.Location = { Position = this.Position; Direction = this.Direction }

type Paths(start) = 
    let mutable map = Map.ofList [ start.Score, [start] ]
    let mutable shortestPaths = Map.ofList [ start.Location, start ]

    // Take advantage of map head always being the lowest score
    // let pop () = 
    //     let (KeyValue(_, head)) = map |> Seq.head
    //     match head with
    //     | [] -> failwith "Paths contained empty list"
    //     | [first] -> 
    //         map <- map |> Map.remove first.Score
    //         first
    //     | first :: tail ->
    //         map <- map |> Map.add first.Score tail
    //         first

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

    
    // member _.Pop() = pop()
    member _.TryPop() = tryPop()
    member _.Push(path) = push path
    member _.Push(paths) = paths |> Array.iter push
    member _.ShortestPaths = shortestPaths
    member _.CurrentPaths = map

let forwardDir (path:Path) =
    match path.Direction with
    | Up -> up path.Position
    | Right -> right path.Position
    | Down -> down path.Position
    | Left -> left path.Position

let tryForward (maze: Maze) (path: Path) = 
    let p1 = forwardDir path
    let l = { Direction = path.Direction; Position = p1 }
    match path.History.Contains(l), maze[p1] with
    | true, _
    | _, Wall -> None
    | _ -> 
        Some { 
            path with
                Score = path.Score + Move.score Forward + (int64 (10 * ((maze.Width - p1.X) + (maze.Height - p1.Y))))
                Position = p1
                // Moves = Forward :: path.Moves
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
                // Moves = Clockwise :: path.Moves
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
                // Moves = Anticlockwise :: path.Moves
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
        match allPaths.TryPop() with
        | Some head ->
            // printfn $"{head.Score} {head.Direction} {head.Position.R}, {head.Position.C}"
            if maze.End = head.Position then Some head
            else
                let newPaths = advance maze head
                allPaths.Push newPaths
                f maze allPaths
        | None -> None

    let start = 
        {
            Score = 0L
            Position = maze.Start
            Direction = Right
            // Moves = []
            History = Set.singleton { Direction = Right; Position = maze.Start }
        }

    let allPaths = Paths(start)
    f maze allPaths

let part1 (input: string) (maxXY: int) numBytes =
    let points = parse input
    let memory = MemorySpace(maxXY)
    memory.DropBytes(points, numBytes)
    memory.DropStartEnd({X = 0; Y = 0}, {X = maxXY; Y = maxXY})
    let mazeInput = render memory
    printfn $"{mazeInput}"
    mazeInput
    |> Maze
    |> findLowestScore
    |> Option.map (fun path ->
        path.History
        |> Set.map _.Position
        |> fun s -> s.Count - 1
    )

part1 sample 6 12

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input 70 1024
#time

let part2 (input: string) (maxXY: int) numBytes =
    let points = parse input
    let memory = MemorySpace(maxXY)
    memory.DropBytes(points, numBytes)
    memory.DropStartEnd({X = 0; Y = 0}, {X = maxXY; Y = maxXY})
    let (Input points) = points
    let rec f i = 
        let p = points[i]
        memory[p] <- Wall
        let mazeInput = render memory
        // printfn $"{points[i]}\n{mazeInput}"
        match findLowestScore (Maze mazeInput) with
        | Some _ -> 
            printfn $"{i}"
            f (i + 1)
        | None -> $"{p.X},{p.Y}"

    f numBytes

// part2 sample 6 12


#time
let result2 = part2 input 70 1024
#time
printfn $"Part1: {result1} Part2: {result2}"
