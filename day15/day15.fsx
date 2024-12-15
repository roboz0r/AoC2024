open System
open System.IO
open System.Collections.Generic

[<Literal>]
let Empty = '.'

[<Literal>]
let Wall = '#'

[<Literal>]
let Box = 'O'

[<Literal>]
let Robot = '@'

[<Literal>]
let Up = '^'

[<Literal>]
let Down = 'v'

[<Literal>]
let Left = '<'

[<Literal>]
let Right = '>'

let sample1 = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"""

let sample2 = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
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


type Warehouse(input: string array) =
    member _.Item with get (r, c) = (input[r][c])
    member _.Item with get (p:Point) = (input[p.R][p.C])

    member _.Width = input[0].Length
    member _.Height = input.Length
    member _.Row(r) = input[r]
    member this.Robot = 
        let rec find r c =
            match input[r][c] with
            | Robot -> { R = r; C = c }
            | _ -> 
                if (c + 1) < this.Width then
                    find r (c + 1)
                elif (r + 1) < this.Height then
                    find (r + 1) 0
                else
                    failwith "No robot"
        find 0 0

type Moves(input: string array) =
    let rec getItem s i =
        let str = input[s]
        if i < str.Length then
            str[i]
        else
            getItem (s + 1) (i - str.Length)

    member _.Item with get (i) = getItem 0 i
    member _.Length = input |> Array.sumBy _.Length
    member _.InputWidth = input[0].Length

type Scenario =
    {
        Warehouse: Warehouse
        Moves: Moves
    }
    
let parse (input: string) =
    let lines = input.Split([| '\r'; '\n'|])
    let warehouse = 
        lines
        |> Array.takeWhile ((<>) "")
        |> Warehouse
    
    let moves =
        lines
        |> Array.skip warehouse.Height
        |> Array.filter (fun x -> x.Length > 0)
        |> Moves
    {
        Warehouse = warehouse
        Moves = moves
    }

let scenario1 = parse sample1

module Scenario =
    let render (x: Scenario) =
        let w = x.Warehouse
        for r in 0 .. (w.Height - 1) do
            for c in 0 .. (w.Width - 1) do
                Console.Write(w[r,c])
            Console.WriteLine()
        let m = x.Moves
        for i in 0 .. (m.Length - 1) do
            if i % m.InputWidth = 0 then 
                Console.WriteLine()
            Console.Write(m[i])
        Console.WriteLine()

type Working = 
    | Working of map:char[,]
    member this.Item 
        with get (r, c) = let (Working map) = this in map[c, r]
        and set (r, c) v = let (Working map) = this in map[c, r] <- v

    member this.Item 
        with get (p:Point) = let (Working map) = this in map[p.C,p.R]
        and set (p:Point) v = let (Working map) = this in map[p.C,p.R] <- v

    member this.Width = let (Working map) = this in Array2D.length1 map
    member this.Height = let (Working map) = this in Array2D.length2 map

module Working = 
    let ofScenario (x: Scenario) =
        let w = x.Warehouse
        Array2D.init w.Width w.Height (fun c r -> w[r,c])
        |> Working

    let swap p1 p2 (working: Working) =
        let p = working[p1]
        working[p1] <- working[p2]
        working[p2] <- p

    let rec tryMove move p (working: Working) =
        let pX = 
            // No need for bounds checks as input is surrounded by walls
            match move with
            | Left -> left p
            | Right -> right p
            | Up -> up p
            | Down -> down p
            | c -> invalidOp $"Unexpected move symbol {c}"

        match working[pX] with
        | Wall -> None
        | Empty -> 
            swap p pX working
            Some pX
        | Box ->
            match tryMove move pX working with
            | Some _ ->
                swap p pX working
                Some pX
            | None -> None

        | c -> invalidOp $"Unexpected warehouse symbol {c}"

    let render (working: Working) =
        for r in 0 .. (working.Height - 1) do
            for c in 0 .. (working.Width - 1) do
                Console.Write(working[r,c])
            Console.WriteLine()

let run beforeMove afterMove (scenario: Scenario) =
    let rec f iMove (robot:Point) (warehouse: Working) =
        if iMove = scenario.Moves.Length then (robot, warehouse)
        else
            let m = scenario.Moves[iMove]
            beforeMove m robot warehouse

            let robot = 
                match Working.tryMove m robot warehouse with
                | Some x -> x
                | None -> robot

            afterMove m robot warehouse
            f (iMove + 1) robot warehouse
    let robot = scenario.Warehouse.Robot
    let working = Working.ofScenario scenario
    f 0 robot working

let printWarehouse m robot warehouse =
    printfn $"Move: %c{m} Robot: {robot.R},{robot.C}"
    Working.render warehouse

let ignoreState _ _ _ = ()

let gpsCoords (working:Working) =
    seq {
        for r in 0 .. (working.Height - 1) do
            for c in 0 .. (working.Width - 1) do
                match working[r,c] with
                | Box -> { R = r; C = c }
                | _ -> ()
    }
    |> Seq.map (fun p -> int64 (100 * p.R + p.C))

let part1 input = 
    let scenario = parse input
    let (robot, wEnd) = run ignoreState ignoreState scenario
    Seq.sum (gpsCoords wEnd)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

[<Literal>]
let BoxLeft = '['

[<Literal>]
let BoxRight = ']'

open System.Text

let expandScenario (scenario: Scenario) = 
    let w = scenario.Warehouse
    let sb = StringBuilder(w.Width * 2)
    let arr = 
        Array.init w.Height (fun r ->
            sb.Clear() |> ignore
            for c in w.Row(r) do
                match c with
                | Wall -> sb.Append(Wall).Append(Wall)
                | Box -> sb.Append(BoxLeft).Append(BoxRight)
                | Empty -> sb.Append(Empty).Append(Empty)
                | Robot -> sb.Append(Robot).Append(Empty)
                | c -> invalidOp $"Unexpected char {c}"
                |> ignore
            sb.ToString()
        )
    { scenario with
        Warehouse = Warehouse(arr)
    }

type MoveTree =
    | End of Point
    | Single of Point * MoveTree
    | Double of (Point * MoveTree) * (Point * MoveTree)

module Working2 = 
    open Working

    let private getMoved move p =
        // No need for bounds checks as input is surrounded by walls
        match move with
        | Left -> left p
        | Right -> right p
        | Up -> up p
        | Down -> down p
        | c -> invalidOp $"Unexpected move symbol {c}"

    let rec tryMove move p (working: Working) =
        let pX = getMoved move p

        match working[pX] with
        | Wall -> None
        | Empty -> 
            Some (Single (p, End pX))
        | BoxLeft ->
            match move with
            | Up | Down ->
                let pL = pX
                let pR = right pX
                match tryMoveBox move pL pR working with
                | Some t ->
                    Some (Single(p, t))
                | None -> None
            | _ ->
                match tryMove move pX working with
                | Some t ->
                    Some (Single(p, t))
                | None -> None
        | BoxRight ->
            match move with
            | Up | Down ->
                let pL = left pX
                let pR = pX
                match tryMoveBox move pL pR working with
                | Some t ->
                    Some (Single(p, t))
                | None -> None
            | _ ->
                match tryMove move pX working with
                | Some t ->
                    Some (Single(p, t))
                | None -> None

        | c -> invalidOp $"Unexpected warehouse symbol {c}"

    and private tryMoveBox move pL pR (working: Working) =
        let pL2 = getMoved move pL
        let pR2 = getMoved move pR

        match working[pL2], working[pR2] with
        | _, Wall -> None
        | Wall, _ -> None
        | Empty, Empty ->
            Some (Double ((pL, End pL2), (pR, End pR2)))

        | BoxLeft, BoxRight ->
            match tryMoveBox move pL2 pR2 working with
            | Some (Double (tL, tR)) ->
                Some (Double ((pL, Single tL), (pR, Single tR)))
            | Some t -> 
                invalidOp $"Unexpected tree {t}"
            | None -> None

        | Empty, BoxLeft ->
            let pLX = pR2
            let pRX = right pR2
            match tryMoveBox move pLX pRX working with
            | Some t ->
                Some (Double ((pL, End pL2), (pR, t)))
            | None -> None

        | BoxRight, Empty ->
            let pLX = left pL2
            let pRX = pL2
            match tryMoveBox move pLX pRX working with
            | Some t ->
                Some (Double ((pL, t), (pR, End pR2)))
            | None -> None

        | BoxRight, BoxLeft ->
            let pLL = left pL2
            let pLR = pL2
            let pRL = pR2
            let pRR = right pR2
            let moveL = tryMoveBox move pLL pLR working
            let moveR = tryMoveBox move pRL pRR working
            match moveL, moveR with
            | Some tL, Some tR ->
                Some (Double ((pL, tL), (pR, tR)))
            | _, None
            | None, _ -> None

        | (c1, c2) -> invalidOp $"Unexpected warehouse symbol {c1}{c2}"

    let doMove (moveSet: HashSet<_>) (t: MoveTree) (w: Working) =
        moveSet.Clear()
        let (p, t) = 
            match t with
            | End _ 
            | Double _ -> invalidOp $"Unexpected tree shape {t}"
            | Single (p, t) -> p, t

        let maybeSwap p pX w = 
            if moveSet.Contains pX then ()
            else 
                moveSet.Add pX |> ignore
                swap p pX w

        let rec f p t =
            match t with
            | End pX ->
                maybeSwap p pX w
                pX
            | Single (pX, tX) ->
                f pX tX |> ignore
                maybeSwap p pX w
                pX
            | Double ((pL, tL), (pR, tR)) ->
                f pL tL |> ignore
                f pR tR |> ignore
                if p.C = pL.C then // Only need to check column as Double only possible for vertical push
                    maybeSwap p pL w
                    pL
                elif p.C = pR.C then
                    maybeSwap p pR w
                    pR
                else
                    invalidOp $"Unaligned tree {p} :: Double (({pL}, {tL}), ({pR}, {tR}))"

        f p t

let run2 beforeMove afterMove (scenario: Scenario) =
    let rec f moveSet iMove (robot:Point) (warehouse: Working) =
        if iMove = scenario.Moves.Length then (robot, warehouse)
        else
            let m = scenario.Moves[iMove]
            beforeMove iMove robot warehouse

            let robot = 
                match Working2.tryMove m robot warehouse with
                | Some tree -> Working2.doMove moveSet tree warehouse
                | None -> robot

            afterMove iMove robot warehouse
            f moveSet (iMove + 1) robot warehouse
    let robot = scenario.Warehouse.Robot
    let working = Working.ofScenario scenario
    f (HashSet()) 0 robot working

let noSplitBoxes iMove robot (warehouse: Working) =
    match warehouse[robot] with
    | Robot -> ()
    | _ -> 
        Working.render warehouse
        invalidOp $"Lost robot at {iMove}"

    for r in 0 .. (warehouse.Height - 1) do
        for c in 0 .. (warehouse.Width - 1) do
            match warehouse[r,c] with
            | BoxLeft ->
                match warehouse[r,c + 1] with
                | BoxRight -> ()
                | _ ->
                    Working.render warehouse
                    invalidOp $"Split box detected %i{iMove}"
            | BoxRight ->
                match warehouse[r,c - 1] with
                | BoxLeft -> ()
                | _ ->
                    Working.render warehouse
                    invalidOp $"Split box detected {iMove}"
            | _ -> ()

let renderI i iMove robot (warehouse: Working) =
    if i = iMove then
        Working.render warehouse

let gpsCoords2 (working:Working) =
    seq {
        for r in 0 .. (working.Height - 1) do
            for c in 0 .. (working.Width - 1) do
                match working[r,c] with
                | BoxLeft -> { R = r; C = c }
                | _ -> ()
    }
    |> Seq.map (fun p -> int64 (100 * p.R + p.C))

let part2 input = 
    let s = parse input
    let s2 = expandScenario s
    // let (robot, wEnd) = run2 (renderI -1) noSplitBoxes s2
    let (robot, wEnd) = run2 ignoreState ignoreState s2
    Seq.sum (gpsCoords2 wEnd)

#time
let result2 = part2 input
#time
printfn $"Part1: {result1} Part2: {result2}"