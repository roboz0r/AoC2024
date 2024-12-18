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

type Maze(input: MemorySpace) =

    let rec findPoint f y x =
        if f (input[x,y]) then 
            { Y = y + 1; X = x + 1 }
        elif (x + 1) < input.Width then 
            findPoint f y (x + 1)
        elif (y + 1) < input.Height then
            findPoint f (y + 1) 0
        else
            failwith "Not found"

    let startP = 
        findPoint ((=) Start) 0 0

    let endP = 
        findPoint ((=) End) 0 0 

    let getPointOrWall x y =
        if x <= 0 || y <= 0 || x > input.Width || y > input.Height then 
            Wall
        else
            input[x - 1, y - 1]

    member _.Item with get (x, y) = getPointOrWall x y
    member _.Item with get (p:Point) = getPointOrWall p.X p.Y

    member _.Width = input.Width + 2
    member _.Height = input.Height + 2

    member this.Start = startP

    member this.End = endP


let renderMaze = 
    let renderBuffer = StringBuilder()
    fun (working: Maze) ->
        renderBuffer.Clear() |> ignore
        for y in 0 .. (working.Height - 1) do
            for x in 0 .. (working.Width - 1) do
                renderBuffer.Append(working[x,y]) |> ignore
            renderBuffer.AppendLine() |> ignore

        renderBuffer.ToString()

type Location =
    {
        Position: Point
    }

type Path =
    {
        Score: int64
        Position: Point
        History: Set<Location>
    }
    member this.Location = { Position = this.Position }

open System.Collections.Generic

type Paths(start) = 
    let pathsByScore = Dictionary()
    let shortestPaths = Dictionary()
    do
        pathsByScore.Add(start.Score, [start])
        shortestPaths.Add(start.Location, start)

    let getMinScore () =
        if pathsByScore.Count = 0 then None
        else
            pathsByScore
            |> Seq.map (fun (KeyValue(score, _)) -> score)
            |> Seq.min 
            |> Some

    let mutable minScore = Some start.Score

    let tryPop () = 
        match minScore |> Option.map (fun score -> score, pathsByScore[score]) with
        | Some (score, head) ->
            match head with
            | [] -> failwith "Paths contained empty list"
            | [first] -> 
                pathsByScore.Remove(score) |> ignore
                minScore <- getMinScore()
                Some first
            | first :: tail ->
                pathsByScore[score] <- tail
                Some first
        | None -> None

    let push (path: Path) =
        let maybePath =
            match shortestPaths.TryGetValue( path.Location) with
            | false, _ -> 
                shortestPaths[path.Location] <- path
                Some path
            | true, p1 ->
                if p1.Score <= path.Score then 
                    None
                else
                    shortestPaths[path.Location] <- path
                    Some path

        match maybePath with
        | Some path ->
            match pathsByScore.TryGetValue(path.Score) with
            | true, paths ->
                let newPaths = 
                    path :: paths
                pathsByScore[path.Score] <- newPaths
            | false, _ -> 
                minScore <- 
                    match minScore with
                    | None -> Some path.Score
                    | Some minScore -> Some (min minScore path.Score)
                pathsByScore[path.Score] <- [path]
        | None -> ()

    member _.TryPop() = tryPop()
    member _.Push(path) = push path
    member _.Push(paths) = paths |> Array.iter push
    member _.ShortestPaths = shortestPaths
    member _.CurrentPaths = pathsByScore

let forwardDir move (path:Path) =
    match move with
    | Up -> up path.Position
    | Right -> right path.Position
    | Down -> down path.Position
    | Left -> left path.Position

let tryMove move (maze: Maze) (path: Path) = 
    let p1 = forwardDir move path
    let l = { Position = p1 }
    match path.History.Contains(l), maze[p1] with
    | true, _
    | _, Wall -> None
    | _ -> 
        Some { 
            path with
                Score = path.Score + 1000L + (int64 (10 * ((maze.Width - p1.X) + (maze.Height - p1.Y))))
                Position = p1
                // Moves = Forward :: path.Moves
                History = path.History.Add l
        }

let moves = [| tryMove Up; tryMove Right; tryMove Down; tryMove Left; |]

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
            History = Set.singleton { Position = maze.Start }
        }

    let allPaths = Paths(start)
    f maze allPaths

let part1 (input: string) (maxXY: int) numBytes =
    let points = parse input
    let memory = MemorySpace(maxXY)
    memory.DropBytes(points, numBytes)
    memory.DropStartEnd({X = 0; Y = 0}, {X = maxXY; Y = maxXY})
    // printfn $"{renderMaze (Maze memory)}"
    memory
    |> Maze
    |> findLowestScore
    |> Option.map (fun path ->
        path.History
        |> Set.map _.Position
        |> fun s -> s.Count - 1
    )

let sampleGridXY, sampleNum = 6, 12
let inputGridXY, inputNum = 70, 1024

// part1 sample sampleGridXY sampleNum

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input inputGridXY inputNum
#time

let part2 (input: string) (maxXY: int) numBytes =
    let points = parse input
    let memory = MemorySpace(maxXY)
    memory.DropBytes(points, numBytes)
    memory.DropStartEnd({X = 0; Y = 0}, {X = maxXY; Y = maxXY})
    let maze = (Maze memory)
    let (Input points) = points
    let rec f i = 
        let p = points[i]
        memory[p] <- Wall
        match findLowestScore maze with
        | Some _ -> 
            // printfn $"{i}"
            f (i + 1)
        | None -> $"{p.X},{p.Y}"

    f numBytes

// part2 sample sampleGridXY sampleNum


#time
let result2 = part2 input inputGridXY inputNum
#time
printfn $"Part1: {result1} Part2: {result2}"
