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
    // |> Seq.map (fun m ->
    //     match moveMap |> Map.tryFind m with
    //     | Some x -> 
    //         // printfn $"{m}: {x}"
    //         x
    //     | None -> failwith $"No such move {m}" )
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
        printfn $"Testing {move} {action}"
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
        printfn $"Testing {move} {action}"
        let robot = testRobot move action
        for c in action do
            robot.Do c

// testNumpadMoves () 
testDirpadMoves ()

let initial = Activate

let getNumpadMoves = moves numpadMoveMap initial
let getDirpadMoves = moves dirpadMoveMap initial

// let moveSet code =
//     getNumpadMoves code
//     |> getDirpadMoves
//     |> getDirpadMoves
//     // |> getDirpadMoves

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
    // printfn $"{code}: {moveSet}"
    // printfn $"{code}: {moveSet.Length} * {codeNum}"
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

// printfn $"Part1: {result1}"
// printfn "---------------------"
// printfn "%s" (moveSetN 0 "4")
// printfn "%s" (moveSetN 1 "4")
// printfn "%s" (moveSetN 2 "4")
// printfn "%s" (moveSetN 3 "4")
// printfn "%s" (moveSetN 4 "4")
// printfn "%s" (moveSetN 5 "4")
// printfn "%s" (moveSetN 6 "4")
// printfn "%s" (moveSetN 7 "4")

// let sets code = 
//     let rec f maps i max = 
//         if i = max then maps
//         else 
//             let map = 
//                 moveSetN i code
//                 |> moveSeq initial
//                 |> Seq.countBy id
//                 |> Seq.map (fun (m, i) -> (m, int64 i) )
//                 |> Map

//             let maps =
//                 maps
//                 |> Map.map (fun move xs ->
//                     match map |> Map.tryFind move with
//                     | Some x -> x :: xs
//                     | None -> 0 :: xs
//                 )

//             f maps (i + 1) max

//     let maps = 
//         dirpadMoveMap
//         |> Map.map (fun move _ -> [])

//     f maps 0 3

// let x = sets "463A"
// x |> Map.iter (fun move seq ->
//     printfn "%A: %A" move seq
// )

// let step (moveCounts: Map<Move, int64 list>) =
//     let emptyMoveMap = 
//         dirpadMoveMap
//         |> Map.map (fun move _ -> 0L)

//     let addOneMove (acc: Map<Move, int64>) move prev = 
//         let action = dirpadMoveMap[move]
//         let actionMoves = 
//             if  action.Length = 1 then seq { Move(action[0], 'A') }
//             else moveSeq (action[0]) (action[1..] + "A")
//         (acc, actionMoves)
//         ||> Seq.fold (fun acc move ->
//             acc |> Map.add move (acc[move] + 1L)
//         )
//         |> Map.map (fun move count -> prev * count)
    
//     let nextCountsByPrevMove =
//         moveCounts
//         |> Map.map (fun move counts ->
//             match counts with
//             | prev :: _ -> 
//                 addOneMove emptyMoveMap move prev
//             | _ -> failwith "No empty lists"
//         )

//     let nextCounts = 
//         emptyMoveMap
//         |> Map.map (fun move _ ->
//             nextCountsByPrevMove
//             |> Map.values
//             |> Seq.sumBy (fun x -> x[move])
//         )

//     moveCounts
//     |> Map.map (fun move counts ->
//         nextCounts[move] :: counts
//     )
    
// sets "463A"
// |> (fun moveCounts ->   
//     moveCounts |> Map.iter (printfn "%A: %A")
//     step moveCounts
// )
// |> (fun moveCounts ->   
//     moveCounts |> Map.iter (printfn "%A: %A")
//     step moveCounts
// )
// |> (fun moveCounts ->   
//     moveCounts |> Map.iter (printfn "%A: %A")
//     step moveCounts
// )

// Move ('<', '<'): [44; 14; 7; 3; 0; 1]
// Move ('<', '>'): [0; 0; 0; 0; 0; 0]
// Move ('<', 'A'): [68; 27; 10; 5; 2; 1]
// Move ('<', '^'): [32; 13; 5; 3; 0; 0]
// Move ('<', 'v'): [37; 18; 6; 2; 2; 0]
// Move ('>', '<'): [0; 0; 0; 0; 0; 0]
// Move ('>', '>'): [27; 10; 5; 2; 1; 1]
// Move ('>', 'A'): [73; 30; 10; 6; 0; 1]
// Move ('>', '^'): [81; 32; 13; 5; 3; 0]
// Move ('>', 'v'): [0; 0; 0; 0; 0; 0]
// Move ('A', '<'): [93; 44; 14; 7; 3; 0]
// Move ('A', '>'): [154; 62; 23; 11; 3; 1]
// Move ('A', 'A'): [45; 21; 9; 4; 3; 0]
// Move ('A', '^'): [30; 10; 6; 0; 1; 1]
// Move ('A', 'v'): [106; 37; 18; 6; 2; 2]
// Move ('^', '<'): [0; 0; 0; 0; 0; 1]
// Move ('^', '>'): [0; 0; 0; 0; 0; 0]
// Move ('^', 'A'): [143; 55; 24; 8; 4; 0]
// Move ('^', '^'): [0; 0; 0; 0; 0; 1]
// Move ('^', 'v'): [0; 0; 0; 0; 0; 0]
// Move ('v', '<'): [44; 14; 7; 3; 1; 0]
// Move ('v', '>'): [0; 0; 0; 0; 0; 0]
// Move ('v', 'A'): [99; 41; 17; 5; 3; 2]
// Move ('v', '^'): [0; 0; 0; 0; 0; 0]
// Move ('v', 'v'): [0; 0; 0; 0; 0; 0]

// ^^<<A>>AvAvA
// <AAv<AA>>^AvAA^A<vA>^A<vA>^A
// v<<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^AA<A>Av<<A>A>^AvA<^A>Av<<A>A>^AvA<^A>A
// <vA<AA>>^AvAA<^A>AAv<<A>A>^Av<<A>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AAv<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A
// v<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AA<vA<AA>>^AvAA<^A>A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A
// <vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A
// v<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A

// ^^<<A
// <AAv<AA>>^A
// v<<A>>^AA<vA<A>>^AAvAA<^A>A
// <vA<AA>>^AvAA<^A>AAv<<A>A>^Av<<A>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^A
// v<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A
// <vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A
// v<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A
// <vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^A

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
    |> Map.ofSeq

// decompose "<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>AA<vA<AA>>^AvA^AvA<^A>A<vA<AA>>^AvAA<^A>AA<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^AAv<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^AAv<<A>A>^AvA<^A>AA<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^Av<<A>^A>AvA^A<vA>^A<A>Av<<A>A>^AvA<^A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>A<vA>^Av<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^Av<<A>^A>AvA^Av<<A>A>^AvA<^A>A<vA<AA>>^AvA<^A>AvA^A<vA>^A<A>Av<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^Av<<A>A>^AvA<^A>Av<<A>>^AvA^A"
// |> Map.iter (printfn "%A: %i")

// let motifs =
//     [|
//         "<A"
//         "<AA>>^A"
//         "<^A"
//         "<vA"
//         ">>^A"
//         ">>^AAvAA<^A"
//         ">A"
//         ">AA<vA"
//         ">AAv<<A"
//         ">^A"
//         ">^AAv<<A"
//         "^A"
//         "^AAv<<A"
//         "v<<A"
//         "vA"
//         "vAA<^A"
//     |]

// let emptyMotifMap = 
//     motifs
//     |> Array.map (fun x -> x, 0L)
//     |> Map

open System.Collections.Generic

let memo (dict: Dictionary<_,_>) k f =
    match dict.TryGetValue k with
    | true, v -> v
    | false, _ ->
        let v = f k
        dict[k] <- v
        v

let nextOne motif prev = 
    getDirpadMoves motif
    |> decompose
    |> Map.map (fun _ count -> prev * count)

let step (motifCounts: Map<string, int64>) =
    motifCounts
    |> Map.map nextOne
    |> Map.values
    |> Seq.collect (fun map ->
        map
        |> Seq.map (fun (KeyValue(k, v)) -> (k, v))
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (k, vs) ->
        let v = vs |> Seq.sumBy snd
        (k, v)
    )
    |> Map

// let runN n (input: string) = 
//     let initialDecomp = decompose (getNumpadMoves input)

//     let f i decomp =
//         if i = n then decomp
//         else
//             f (i + 1) (step decomp)

//     initialDecomp
//     |> step
//     |> step

let runN n (input: string) = 
    let initialDecomp = decompose (getNumpadMoves input)

    let rec f i decomp =
        if i = n then decomp
        else
            f (i + 1) (step decomp)

    f 0 initialDecomp


// let sample  = 
//     """029A
// 980A
// 179A
// 456A
// 379A
// """
// 463A
// 340A
// 129A
// 083A
// 341A

// let m1 = decompose ("341A" |> getNumpadMoves |> getDirpadMoves |> getDirpadMoves |> getDirpadMoves)
// let m2 = runN 3 "341A"

// m1 = m2


let complexity2 (code: string) (moveSet: Map<string, int64>) =
    let codeNum = int (code.TrimEnd('A'))
    // printfn $"{code}: {moveSet}"
    // printfn $"{code}: {moveSet.Length} * {codeNum}"
    let value = 
        moveSet 
        |> Seq.sumBy (fun (KeyValue(s, count)) ->
            (int64 s.Length) * count
        )
    value * int64 codeNum


let part2 (input: string) =
    let decompCounts code = runN 25 code

    let codes = input.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
    codes
    |> Array.sumBy (fun code ->
        let moveSet = decompCounts code
        complexity2 code moveSet
    )

#time
//178023611514396
//135019550868544
let result2 = part2 input
#time

printfn $"Part1: {result1} Part2: {result2}"
