open System
open System.IO
open System.Text

[<Literal>]
let EmptyInput = '.'

[<Literal>]
let WallInput = '#'

[<Literal>]
let StartInput = 'S'

[<Literal>]
let EndInput = 'E'


[<Literal>]
let Empty = -1

[<Literal>]
let Wall = -2

[<Literal>]
let Start = 0

[<Literal>]
let End = -3

let sample = 
    """###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"""


[<Struct>]
type Point = 
    {
        X: int
        Y: int
    }

type Input(grid: int[,]) =

    member this.Item 
        with get (x, y) = grid[x, y]
        and set (x, y) v = grid[x, y] <- v

    member this.Item 
        with get (p:Point) = grid[p.X,p.Y]
        and set (p:Point) v = grid[p.X,p.Y] <- v

    member this.Width = Array2D.length1 grid
    member this.Height = Array2D.length2 grid

let parse (input: string) = 
    let rows = input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

    Array2D.init (rows[0].Length) rows.Length (fun x y ->
        match rows[y][x] with
        | EmptyInput -> Empty
        | WallInput -> Wall
        | StartInput -> Start
        | EndInput -> End
        | c -> failwith $"Unexpected input char {c}"
    )
    |> Input

let intToChar =
    function
    | Wall -> WallInput
    | Empty -> EmptyInput
    | Start -> StartInput
    | End -> EndInput
    | i ->
        let i = i % 32
        if i >= 0 && i <= 9 then char (i + 48)
        else char (i + 87)


let render = 
    let renderBuffer = StringBuilder()
    fun (working: Input) ->
        renderBuffer.Clear() |> ignore
        for y in 0 .. (working.Height - 1) do
            for x in 0 .. (working.Width - 1) do
                renderBuffer.Append(intToChar working[x,y]) |> ignore
            renderBuffer.AppendLine() |> ignore
        renderBuffer.ToString()

sample
|> parse
|> render
|> (printfn "%s")
    
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

type Maze(input: Input) =

    let rec findPoint f y x =
        if f (input[x,y]) then 
            { Y = y; X = x }
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
        if x < 0 || y < 0 || x >= input.Width || y >= input.Height then 
            Wall
        else
            input[x, y]

    member _.Item with get (x, y) = getPointOrWall x y
    member _.Item with get (p:Point) = getPointOrWall p.X p.Y

    member _.Width = input.Width
    member _.Height = input.Height

    member this.Start = startP

    member this.End = endP
    member this.Input = input

let renderToConsole x = render x |> (printfn "%s")

let moves = [| up; down; left; right |]

let getNextPoint (maze: Maze) p =
    moves
    |> Array.choose (fun move ->
        let p1 = move p
        match maze[p1] with
        | Empty | End -> Some p1
        | _ -> None
    )
    |> function
    | [| pNext |] -> pNext
    | xs -> 
        renderToConsole maze.Input
        failwith $"Unexpected mulitple moves from {p} {xs}"

let fillInput (input: Input) = 
    let maze = Maze input
    let rec f p i = 
        let pNext = getNextPoint maze p
        match input[pNext] with
        | End -> 
            input[pNext] <- i
            maze
        | Empty ->
            input[pNext] <- i
            f pNext (i + 1)
        | iInput ->
            renderToConsole maze.Input
            failwith $"Unexpected value filling input {pNext} {iInput}"

    f maze.Start 1

let tee f x = f x; x

sample
|> parse
|> tee (render >> (printfn "%s"))
|> fillInput
|> _.Input |> (render >> (printfn "%s"))

[<Struct>]
type Cheat =
    {
        Wall: Point
        Direction: Direction
    }

let cheat x y d = 
    {
        Wall = { X = x; Y = y }
        Direction = d
    }

let tryCheat (maze: Maze) (cheat: Cheat) = 
    let pStart, pEnd =
        let p = cheat.Wall
        match cheat.Direction with
        | Up ->
            down p, up p
        | Down ->
            up p, down p
        | Left ->
            right p, left p
        | Right ->
            left p, right p

    match maze[pStart], maze[pEnd] with
    | Wall, _
    | _, Wall -> None
    | Empty, _
    | _, Empty
    | End, _
    | _, End -> 
        renderToConsole maze.Input
        failwith $"Unexpected unpopulated maze {pStart.X}, {pStart.Y}:{maze[pStart]}, {pEnd.X}, {pEnd.Y}:{maze[pEnd]}"
    | iStart, iEnd ->
        let v = iEnd - iStart - 2
        if v > 0 then 
            Some (cheat, v)
        else
            None

let possibleCheats (maze: Maze) =
    seq {
        // -2 as we can't cheat through the outer wall
        for y in 1 .. (maze.Height - 2) do
            for x in 1 .. (maze.Width - 2) do
                match maze[x, y] with
                | Wall ->
                    cheat x y Up
                    cheat x y Down
                    cheat x y Left
                    cheat x y Right
                | _ -> ()
    }

let part1 input atLeast = 
    let parsed = parse input
    let maze = fillInput parsed
    maze
    |> possibleCheats
    |> Seq.choose (tryCheat maze)
    |> Seq.filter (fun (cheat, i) -> i >= atLeast)
    |> Seq.length

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input 100
#time

let possibleCheatsI maxDist =
    [|
        for dy in -maxDist .. maxDist do      
            let x = maxDist - abs dy
            for dx in -x .. x do
                struct (dx, dy)
    |]

let dist x y = (abs x) + (abs y)
let tryCheat2 (maze: Maze) atLeast (pStart) (struct (dx, dy)) = 
    let pEnd = { X = pStart.X + dx; Y = pStart.Y + dy }

    match maze[pStart], maze[pEnd] with
    | Wall, _
    | _, Wall -> 0
    | iStart, iEnd ->
        let v = iEnd - iStart - (dist dx dy)
        if v >= atLeast then 
            1
        else
            0

let pointsFromEnd (maze: Maze) atLeast iEnd = 
    [|
        // -2 as no track on outer wall
        for y in 1 .. (maze.Height - 2) do
            for x in 1 .. (maze.Width - 2) do
                match maze[x, y] with
                | Wall -> ()
                | i ->
                    if i < (iEnd - atLeast) then 
                        { X = x; Y = y }
    |]

let part2 input atLeast maxDist = 
    let parsed = parse input
    let maze = fillInput parsed
    let trackPoints = pointsFromEnd maze atLeast (maze[maze.End])
    let possibleCheats = possibleCheatsI maxDist
    trackPoints
    |> Array.Parallel.sumBy  (fun pStart ->
        possibleCheats
        |> Array.sumBy (tryCheat2 maze atLeast pStart)
    )

// part2 sample 50

#time
let result1_2, result2 = part2 input 100 2, part2 input 100 20
#time
printfn $"Part1: {result1} or {result1_2} Part2: {result2}"
