open System
open System.IO

let sample  = 
    """029A
980A
179A
456A
379A
"""

let permutations choices len =
    seq {
        let numChoices = Array.length choices
        for i in 0..(pown numChoices len - 1) do
            yield Array.init len (fun j -> choices[(i / (pown numChoices j )) % numChoices] )
    }

let numpad = "0123456789A"
let directions = "^<v>A"

// type Codes = string[]

let numpadMoves = permutations (numpad.ToCharArray()) 2
let dirpadMoves = permutations (directions.ToCharArray()) 2

// numpadMoves
// |> Seq.iter (fun xs -> printfn $"        '{xs[0]}', '{xs[1]}', \"\"")

// dirpadMoves
// |> Seq.iter (fun xs -> printfn $"        '{xs[0]}', '{xs[1]}', \"\"")

type Move = Move of from:char * dest:char

let numpadMoveMap =
    [|
        '0', '0', "A"
        '1', '0', ">vA"
        '2', '0', "vA"
        '3', '0', "<vA"
        '4', '0', ">vvA"
        '5', '0', "vvA"
        '6', '0', "<vvA"
        '7', '0', ">vvvA"
        '8', '0', "vvvA"
        '9', '0', "<vvvA"
        'A', '0', "<A"
        '0', '1', "^<A"
        '1', '1', "A"
        '2', '1', "<A"
        '3', '1', "<<A"
        '4', '1', "vA"
        '5', '1', "<vA"
        '6', '1', "<<vA"
        '7', '1', "vvA"
        '8', '1', "<vvA"
        '9', '1', "<<vvA"
        'A', '1', "^<<A"
        '0', '2', "^A"
        '1', '2', ">A"
        '2', '2', "A"
        '3', '2', "<A"
        '4', '2', ">vA"
        '5', '2', "vA"
        '6', '2', "<vA"
        '7', '2', ">vvA"
        '8', '2', "vvA"
        '9', '2', "<vvA"
        'A', '2', "<^A"
        '0', '3', ">^A"
        '1', '3', ">>A"
        '2', '3', ">A"
        '3', '3', "A"
        '4', '3', ">>vA"
        '5', '3', ">vA"
        '6', '3', "vA"
        '7', '3', ">>vvA"
        '8', '3', "vv>A" //">vvA"
        '9', '3', "vvA"
        'A', '3', "^A"
        '0', '4', "^^<A"
        '1', '4', "^A"
        '2', '4', "<^A"
        '3', '4', "<<^A" //"^<<A"
        '4', '4', "A"
        '5', '4', "<A"
        '6', '4', "<<A"
        '7', '4', "vA"
        '8', '4', "<vA"
        '9', '4', "<<vA"
        'A', '4', "^^<<A"
        '0', '5', "^^A"
        '1', '5', ">^A"
        '2', '5', "^A"
        '3', '5', "<^A"
        '4', '5', ">A"
        '5', '5', "A"
        '6', '5', "<A"
        '7', '5', ">vA"
        '8', '5', "vA"
        '9', '5', "<vA"
        'A', '5', "<^^A"
        '0', '6', "^^>A"
        '1', '6', "^>>A"
        '2', '6', "^>A"
        '3', '6', "^A"
        '4', '6', ">>A"
        '5', '6', ">A"
        '6', '6', "A"
        '7', '6', ">>vA"
        '8', '6', ">vA"
        '9', '6', "vA"
        'A', '6', "^^A"
        '0', '7', "^^^<A"
        '1', '7', "^^A"
        '2', '7', "<^^A"
        '3', '7', "<<^^A"
        '4', '7', "^A"
        '5', '7', "<^A"
        '6', '7', "<<^A"
        '7', '7', "A"
        '8', '7', "<A"
        '9', '7', "<<A"
        'A', '7', "^^^<<A"
        '0', '8', "^^^A"
        '1', '8', ">^^A"
        '2', '8', "^^A"
        '3', '8', "<^^A"
        '4', '8', ">^A"
        '5', '8', "^A"
        '6', '8', "<^A"
        '7', '8', ">A"
        '8', '8', "A"
        '9', '8', "<A"
        'A', '8', "^^^<A"
        '0', '9', ">^^^A"
        '1', '9', ">>^^A"
        '2', '9', "^^>A" // ">^^A" equivalent
        '3', '9', "^^A"
        '4', '9', ">>^A"
        '5', '9', ">^A"
        '6', '9', "^A"
        '7', '9', ">>A"
        '8', '9', ">A"
        '9', '9', "A"
        'A', '9', "^^^A"
        '0', 'A', ">A"
        '1', 'A', ">>vA"
        '2', 'A', ">vA"
        '3', 'A', "vA"
        '4', 'A', ">>vvA"
        '5', 'A', ">vvA"
        '6', 'A', "vvA"
        '7', 'A', ">>vvvA"
        '8', 'A', ">vvvA"
        '9', 'A', "vvvA"
        'A', 'A', "A"
    |]
    |> Array.map (fun (from, dest, move) ->
        (Move (from, dest), move)
    )
    |> Map

let dirpadMoveMap = 
    [|
        '^', '^', "A"
        '<', '^', ">^A"
        'v', '^', "^A"
        '>', '^', "<^A" // "^<A" higher
        'A', '^', "<A"
        '^', '<', "v<A" // fixed
        '<', '<', "A"
        'v', '<', "<A"
        '>', '<', "<<A"
        'A', '<', "v<<A" //"<v<A" higher
        '^', 'v', "vA"
        '<', 'v', ">A"
        'v', 'v', "A"
        '>', 'v', "<A"
        'A', 'v', "<vA" // "v<A" higher
        '^', '>', "v>A" //">vA" higher
        '<', '>', ">>A"
        'v', '>', ">A"
        '>', '>', "A"
        'A', '>', "vA"
        '^', 'A', ">A"
        '<', 'A', ">>^A"
        'v', 'A', "^>A" //">^A" higher
        '>', 'A', "^A"
        'A', 'A', "A"
    |]
    |> Array.map (fun (from, dest, move) ->
        (Move (from, dest), move)
    )
    |> Map

let moveSeq start (required: string) =
    seq {
        Move(start, required[0])
        for i in 1 .. (required.Length - 1) do
            Move(required[i - 1], required[i])
    }

let moves (moveMap: Map<Move, string>) start (required: string) =
    required
    |> moveSeq start
    |> Seq.map (fun m -> moveMap[m])
    |> String.concat ""

[<Literal>]
let Up = '^'

[<Literal>]
let Down = 'v'

[<Literal>]
let Left = '<'

[<Literal>]
let Right = '>'

[<Literal>]
let Activate = 'A'

type NumericRobot(start, activate) = 
    let mutable current = start

    member _.Current = current
    member _.Do(move) =
        match move with
        | Up ->
            match current with 
            | '1' | '2' | '3'
            | '4' | '5' | '6' -> current <- current + (char 3)
            | '0' -> current <- current + (char 2)
            | 'A' -> current <- '3'
            | '7' | '8' | '9' -> invalidOp $"Attempted to move up from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Down ->
            match current with 
            | '7' | '8' | '9' 
            | '4' | '5' | '6' -> current <- current - (char 3)
            | '2' -> current <- '0'
            | '3'-> current <- 'A'
            | '1' | '0' | 'A' ->  invalidOp $"Attempted to move down from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Left ->
            match current with 
            | '8' | '5' | '2'
            | '9' | '6' | '3' -> current <- current - (char 1)
            | 'A' -> current <- '0'
            | '7' | '4' | '1' | '0' -> invalidOp $"Attempted to move left from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Right ->
            match current with 
            | '8' | '5' | '2'
            | '7' | '4' | '1' -> current <- current + (char 1)
            | '0' -> current <- 'A'
            | '9' | '6' | '3' | 'A' -> invalidOp $"Attempted to move right from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Activate -> activate current
        | _ -> failwith $"Unexpected move {move}"

type DirectionalRobot(start, activate) = 
    let mutable current = start
    member _.Current = current
    member _.Do(move) =
        match move with
        | Up ->
            match current with
            | Down -> current <- Up
            | Right -> current <- Activate
            | Left | Up | Activate -> invalidOp $"Attempted to move up from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Down ->
            match current with
            | Up -> current <- Down
            | Activate -> current <- Right
            | Left | Down | Right -> invalidOp $"Attempted to move down from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Left ->
            match current with
            | Activate -> current <- Up
            | Right -> current <- Down
            | Down -> current <- Left
            | Up | Left -> invalidOp $"Attempted to move left from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Right ->
            match current with
            | Up -> current <- Activate
            | Down -> current <- Right
            | Left -> current <- Down
            | Activate | Right -> invalidOp $"Attempted to move right from {current}"
            | _ -> failwith $"Unexpected current {current}"
        | Activate -> activate current
        | _ -> failwith $"Unexpected move {move}"


let testNumpadMoves () = 
    let testRobot (Move (from, dest)) action = NumericRobot(from, fun c ->
        if dest = c then ()
        else
            invalidOp $"Invalid action {action} expected {from}->{dest} but got {c}"
    )

    for (KeyValue(move, action)) in numpadMoveMap do
        // printfn $"Testing {move} {action}"
        let robot = testRobot move action
        for c in action do
            robot.Do c

let testDirpadMoves () = 
    let testRobot (Move (from, dest)) action = DirectionalRobot(from, fun c ->
        if dest = c then ()
        else
            invalidOp $"Invalid action {action} expected {from}->{dest} but got {c}"
    )

    for (KeyValue(move, action)) in dirpadMoveMap do
        // printfn $"Testing {move} {action}"
        let robot = testRobot move action
        for c in action do
            robot.Do c

testNumpadMoves () 
testDirpadMoves ()

let initial = Activate

let getNumpadMoves = moves numpadMoveMap initial
let getDirpadMoves = moves dirpadMoveMap initial

let moveSetN n code =
    let rec f i moveSet = 
        if i = 0 then moveSet
        else
            f (i - 1) (getDirpadMoves moveSet)
    f n (getNumpadMoves code)

let testMoveSet (code: string) (moveSet: string) =

    let robot = NumericRobot(initial, printfn "Press: %c")
    let robot1 = DirectionalRobot(initial, robot.Do)
    let robot2 = DirectionalRobot(initial, robot1.Do)

    printfn $"Testing {code}: {moveSet}"

    for c in moveSet do
        robot2.Do c


let complexity (code: string) (moveSet: string) =
    let codeNum = int (code.TrimEnd('A'))
    int64 moveSet.Length * int64 codeNum

let part1 (input: string) =
    let moveSet code = moveSetN 2 code

    let codes = input.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    codes
    |> Array.sumBy (fun code ->
        let moveSet = moveSet code
        // testMoveSet code moveSet
        complexity code moveSet
    )

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
//94426
let result1 = part1 input
#time

let decompose (s: string) = 
    let rec nextSingleA i =
        match s[i] with
        | Activate ->
            if i = s.Length - 1 then i
            else
                match s[i + 1] with
                | Activate -> nextSingleA (i + 2)
                | _ -> i
        | _ -> nextSingleA (i + 1)

    let rec f i =
        seq {
            let j = nextSingleA i
            s[i..j]
            if j = s.Length - 1 then ()
            else
                yield! f (j + 1)
        }

    f 0
    |> Seq.countBy id
    |> Seq.map (fun (s, i) -> (s, int64 i))
    |> Array.ofSeq

let nextOne (motif, prev) = 
    getDirpadMoves motif
    |> decompose
    |> Array.map (fun (s, count) -> s, prev * count)

let step (motifCounts: array<string * int64>) =
    motifCounts
    |> Array.map nextOne
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (k, vs) ->
        let v = vs |> Seq.sumBy snd
        (k, v)
    )
    |> Array.ofSeq

let runN n (input: string) = 
    let rec f i decomp =
        if i = n then decomp
        else
            f (i + 1) (step decomp)

    let initialDecomp = decompose (getNumpadMoves input)
    f 0 initialDecomp

let complexity2 (code: string) (moveSet: array<string * int64>) =
    let codeNum = int64 (code.TrimEnd('A'))
    let value = 
        moveSet 
        |> Seq.sumBy (fun (s, count) -> (int64 s.Length) * count)
    value * codeNum

let part2 n (input: string) =
    let decompCounts code = runN n code
    let codes = input.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    codes
    |> Array.sumBy (fun code ->
        let moveSet = decompCounts code
        complexity2 code moveSet
    )

#time
//118392478819140
let result1_2, result2 = part2 2 input, part2 25 input
#time

printfn $"Part1: {result1} or {result1_2} Part2: {result2}"
