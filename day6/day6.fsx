open System
open System.IO

let sample = 
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""


[<Literal>]
let GuardStart = '^'

[<Literal>]
let Obstruction = '#'

[<Literal>]
let Empty = '.'

[<Struct>]
type Point(r: int, c: int) =
    member _.R = r
    member _.C = c


type Direction =
    | Up
    | Down
    | Left
    | Right

[<Struct>]
type Location(p: Point, d: Direction) =
    member _.Point = p
    member _.Direction = d


module Direction =
    // always turns clockwise
    let turn = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

type LabMap(input: string array) =
    let start = 
        input
        |> Array.indexed
        |> Array.pick (fun (r, s) ->
            match s.IndexOf(GuardStart) with
            | -1 -> None
            | c -> Point(r, c) |> Some
        )

    member _.Item with get (r, c) = input[r][c]
    member _.Item with get (p:Point) = input[p.R][p.C]
    // member _.Item with get (p:Point) = input[p.X][p.Y]
    member _.Item with get (r) = input[r]
    member _.Start = start

    member _.Width = input[0].Length
    member _.Height = input.Length

let parse (input: string) =
    input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> LabMap

[<Struct>]
type LabState =
    {
        Map: LabMap
        Visited: Set<Point>
        Current: Location
        OnMap: bool
    }

type Action =
    | Moved of Location
    | Turned of Location
    | LeftMap

module LabState =
    let ofMap map =
        {
            Map = map
            Visited = set [map.Start]
            Current = Location(map.Start, Up)
            OnMap = true
        }

    let private nextAction state =
        let p = state.Current.Point
        let d = state.Current.Direction
        
        match d with
        | Up ->
            if p.R = 0 then
                LeftMap
            else
                let nextP = Point(p.R - 1, p.C)
                match state.Map[nextP] with
                | Obstruction -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Down ->
            if p.R = state.Map.Height - 1 then
                LeftMap
            else
                let nextP = Point(p.R + 1, p.C)
                match state.Map[nextP] with
                | Obstruction -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Right ->
            if p.C = state.Map.Width - 1 then
                LeftMap
            else
                let nextP = Point(p.R, p.C + 1)
                match state.Map[nextP] with
                | Obstruction -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Left ->
            if p.C = 0 then
                LeftMap
            else
                let nextP = Point(p.R, p.C - 1)
                match state.Map[nextP] with
                | Obstruction -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))

    let private advance state =
        match nextAction state with
        | Moved l ->
            { state with
                Current = l
                Visited = state.Visited |> Set.add l.Point
            }
        | Turned l ->
            { state with
                Current = l
            }
        | LeftMap ->
            { state with
                OnMap = false
            }

    let runToExit map =
        let rec f state = 
            let nextState = advance state
            if state.OnMap then
                f nextState
            else 
                state
        f (ofMap map)

let part1 input =
    input
    |> parse
    |> LabState.runToExit
    |> (fun x -> x.Visited.Count)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
#time
let result1 = part1 input
#time

type WorkingState =
    | DidLoop
    | Exited
    | Working

open System.Collections.Generic

[<Struct>]
type LabState2 =
    {
        Map: LabMap
        Obstruction: Point
        History: HashSet<Location>
        Current: Location
        WorkingState: WorkingState
    }

[<RequireQualifiedAccess>]
type Action2 =
    | Moved
    | Turned
    | LeftMap

module LabState2 =
    let ofMap map obstruction =
        {
            Map = map
            Obstruction = obstruction
            History = HashSet [Location(map.Start, Up)]
            Current = Location(map.Start, Up)
            WorkingState = Working
        }

    let (|Obstructed|_|) (state: LabState2, nextP: Point) =
        state.Map[nextP] = Obstruction || (nextP = state.Obstruction)

    let private nextAction state =
        let p = state.Current.Point
        let d = state.Current.Direction
        
        match d with
        | Up ->
            if p.R = 0 then
                LeftMap
            else
                let nextP = Point(p.R - 1, p.C)
                match state, nextP with
                | Obstructed -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Down ->
            if p.R = state.Map.Height - 1 then
                LeftMap
            else
                let nextP = Point(p.R + 1, p.C)
                match state, nextP with
                | Obstructed -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Right ->
            if p.C = state.Map.Width - 1 then
                LeftMap
            else
                let nextP = Point(p.R, p.C + 1)
                match state, nextP with
                | Obstructed -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))
        | Left ->
            if p.C = 0 then
                LeftMap
            else
                let nextP = Point(p.R, p.C - 1)
                match state, nextP with
                | Obstructed -> Turned (Location(p, Direction.turn d))
                | _ -> Moved (Location(nextP, d))

    let private nextAction2 state =
        let p = state.Current.Point
        let d = state.Current.Direction
        
        match d with
        | Up ->
            if p.R = 0 then
                struct(Action2.LeftMap, Unchecked.defaultof<_>)
            else
                let nextP = Point(p.R - 1, p.C)
                match state, nextP with
                | Obstructed -> struct(Action2.Turned,  (Location(p, Direction.turn d)))
                | _ -> struct(Action2.Moved, (Location(nextP, d)))
        | Down ->
            if p.R = state.Map.Height - 1 then
                struct(Action2.LeftMap, Unchecked.defaultof<_>)
            else
                let nextP = Point(p.R + 1, p.C)
                match state, nextP with
                | Obstructed -> struct(Action2.Turned,  (Location(p, Direction.turn d)))
                | _ -> struct(Action2.Moved, (Location(nextP, d)))
        | Right ->
            if p.C = state.Map.Width - 1 then
                struct(Action2.LeftMap, Unchecked.defaultof<_>)
            else
                let nextP = Point(p.R, p.C + 1)
                match state, nextP with
                | Obstructed -> struct(Action2.Turned,  (Location(p, Direction.turn d)))
                | _ -> struct(Action2.Moved, (Location(nextP, d)))
        | Left ->
            if p.C = 0 then
                struct(Action2.LeftMap, Unchecked.defaultof<_>)
            else
                let nextP = Point(p.R, p.C - 1)
                match state, nextP with
                | Obstructed -> struct(Action2.Turned,  (Location(p, Direction.turn d)))
                | _ -> struct(Action2.Moved, (Location(nextP, d)))

    let private advance state =
        let history = state.History
        match nextAction state with
        | Moved l
        | Turned l ->
            if history.Contains l then 
                { state with
                    Current = l
                    WorkingState = DidLoop
                }
            else
                state.History.Add l |> ignore
                { state with
                    Current = l
                    // History = state.History |> Set.add l
                }
        | LeftMap ->
            { state with
                WorkingState = Exited
            }

    let private advance2 state =
        let history = state.History
        match nextAction2 state with
        | Action2.Moved,  l
        | Action2.Turned, l ->
            if history.Contains l then 
                { state with
                    Current = l
                    WorkingState = DidLoop
                }
            else
                state.History.Add l |> ignore
                { state with
                    Current = l
                    // History = state.History |> Set.add l
                }
        | Action2.LeftMap, _ ->
            { state with
                WorkingState = Exited
            }

    let runToExitOrLoop map obstruction =
        let rec f state = 
            let nextState = advance state
            if state.WorkingState = Working then
                f nextState
            else 
                state
        f (ofMap map obstruction)

    [<TailCall>]
    let rec run2 state = 
        let nextState = advance2 state
        if state.WorkingState = Working then
            run2 nextState
        else 
            state

    let runToExitOrLoop2 map obstruction =
        run2 (ofMap map obstruction)

    let findAllObstructions (map: LabMap) =
        let nextRC r c =
            if c = map.Width - 1 then 
                if r = map.Height - 1 then 
                    None
                else
                    Some(r + 1, 0)
            else
                Some(r, c + 1)

        let rec f loopObstructions r c =
            let loopObstructions =
                match map[r, c] with
                | Obstruction
                | GuardStart -> loopObstructions
                | _ ->
                    let obstruction = Point(r, c)
                    let state = runToExitOrLoop map obstruction
                    match state.WorkingState with
                    | Working -> failwith "Didnt exit or loop"
                    | DidLoop -> loopObstructions |> Set.add (Point(r, c))
                    | Exited -> loopObstructions
                    
            match nextRC r c with
            | Some(r, c) -> f loopObstructions r c
            | None -> loopObstructions

        f Set.empty 0 0

    [<TailCall>]
    let rec f (map: LabMap) (loopObstructions: HashSet<Point>) (p: Point) visited =
        let r, c = p.R, p.C
        match map[r, c] with
        | Obstruction
        | GuardStart -> ()
        | _ ->
            let obstruction = Point(r, c)
            let state = runToExitOrLoop2 map obstruction
            match state.WorkingState with
            | Working -> failwith "Didnt exit or loop"
            | DidLoop -> loopObstructions.Add (Point(r, c)) |> ignore
            | Exited -> ()
                
        match visited with
        | p :: visited ->
            f map loopObstructions p visited
        | _ -> loopObstructions

    let findAllObstructions2 (map: LabMap) =
        let visited = 
            map
            |> LabState.runToExit
            |> _.Visited
            |> List.ofSeq

        match visited with
        | p :: visited ->
            f map (HashSet<Point>()) p visited
        | _ -> HashSet<Point>()
            
# time
let part2 input =
    input
    |> parse
    |> LabState2.findAllObstructions
    |> _.Count

let result2 = part2 input
        
# time
let part2_2 input =
    input
    |> parse
    |> LabState2.findAllObstructions2
    |> _.Count

let result2_2 = part2_2 input
# time