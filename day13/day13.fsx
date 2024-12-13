#r "nuget: FParsec"
open System
open System.IO

type Button =
    | A
    | B

[<Measure>]
type tk

let price = 
    function
    | A -> 3L<tk>
    | B -> 1L<tk>

// Prizes on an x,y grid

[<Struct>]
type Point =
    {
        X: int64
        Y: int64
    }

let sample = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

type Scenario =
    {
        AMove: Point
        BMove: Point
        Prize: Point
    }

module Scenario =
    open FParsec
    let pAMove = 
        pipe5 (pstring "Button A: X+") pint32 (pstring ", Y+") pint32 newline
            (fun _ x _ y _ -> { X = x; Y = y })
    let pBMove = 
        pipe5 (pstring "Button B: X+") pint32 (pstring ", Y+") pint32 newline
            (fun _ x _ y _ -> { X = x; Y = y })
    let pPrize = 
        pipe5 (pstring "Prize: X=") pint32 (pstring ", Y=") pint32 newline
            (fun _ x _ y _ -> { X = x; Y = y })

    let pScenario =
        pipe3 pAMove pBMove pPrize (fun a b p -> { AMove = a; BMove = b; Prize = p })

    let parse (input: string) =
        
        match run ((sepBy pScenario newline) .>> eof) input with
        | Success (s, _, _) -> s
        | Failure (s, err, _) ->
            failwithf "%s %A" s err

sample
|> Scenario.parse

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))


input
|> Scenario.parse

type Solution =
    {
        ACount: int64
        BCount: int64
    }

module Solution =
    let cost solution =
        (solution.ACount * price A)
        + (solution.BCount * price B)

let maxPress = 100L

let trySolve (scenario: Scenario) =
    let { X = xPrize; Y = yPrize } = scenario.Prize
    let checkY aPress bPress =
        let ay = aPress * scenario.AMove.Y
        let by = bPress * scenario.BMove.Y
        if ay + by = yPrize then 
            Some { ACount = aPress; BCount = bPress } 
        else
            None
                
    let rec findSol aPress =
        if aPress < 0L then None
        else
            let ax = aPress * scenario.AMove.X
            let rem = xPrize - ax
            match rem % scenario.BMove.X with
            | 0L -> 
                let bPress = rem / scenario.BMove.X
                if bPress <= maxPress then
                    match checkY aPress bPress with
                    | Some solution -> Some solution
                    | None -> findSol (aPress - 1L)
                else
                    findSol (aPress - 1L)
            | _ ->
                    findSol (aPress - 1L)

    let rec solutions aPress = 
        seq {
            match findSol aPress with
            | Some sol -> 
                sol
                yield! solutions (sol.ACount - 1L)
            | None -> ()
        }

    let aPress = min maxPress (xPrize / scenario.AMove.X)
    solutions aPress

let part1 input =
    input
    |> Scenario.parse
    |> List.map trySolve
    |> List.sumBy (fun solutions ->
        if Seq.isEmpty solutions then
            0L<tk>
        else
            (solutions |> Seq.minBy Solution.cost |> Solution.cost )
    )

#time
let result1 = part1 input
#time


// aPress * axMove + bPress * bxMove = xPrize
// aPress * ayMove + bPress * byMove = yPrize

// Solve system of equations
// aPress * axMove + ((yPrize - (aPress * ayMove)) / byMove) * bxMove = xPrize
// aPress * axMove + (yPrize * (bxMove/ byMove)) - ((aPress * ayMove) * (bxMove/ byMove)) = xPrize
// aPress * axMove - ((aPress * ayMove) * (bxMove/ byMove)) = xPrize - (yPrize * (bxMove/ byMove))
// aPress * axMove - ((aPress * ayMove * bxMove) / byMove) = xPrize - (yPrize * (bxMove/ byMove))
// (aPress * axMove * byMove) / byMove - ((aPress * ayMove * bxMove) / byMove) = xPrize - (yPrize * (bxMove/ byMove))
// (aPress * axMove * byMove) - (aPress * ayMove * bxMove) = (xPrize - (yPrize * (bxMove/ byMove))) * byMove
// (aPress * axMove * byMove) - (aPress * ayMove * bxMove) = xPrize * byMove - yPrize * bxMove 
// aPress * ((axMove * byMove) - (ayMove * bxMove)) = xPrize * byMove - yPrize * bxMove 

// aPress = (xPrize * byMove - yPrize * bxMove) / ((axMove * byMove) - (ayMove * bxMove))

let trySolve2 scenario = 
    let { X = axMove; Y = ayMove } = scenario.AMove
    let { X = bxMove; Y = byMove } = scenario.BMove
    let { X = xPrize; Y = yPrize } = scenario.Prize

    let num = xPrize * byMove - yPrize * bxMove
    let div = axMove * byMove - ayMove * bxMove
    match num % div with
    | 0L -> 
        let aPress = num / div
        let bPress = (xPrize - aPress * axMove) / bxMove
        Some ({ ACount = aPress; BCount = bPress })
    | _ -> None

let offset = 10_000_000_000_000L

let part2 input =
    input
    |> Scenario.parse
    |> List.map (fun s -> 
        { s with 
            Prize.X = s.Prize.X + offset
            Prize.Y = s.Prize.Y + offset
        })
    |> List.map trySolve2
    |> List.sumBy (fun solution ->
        match solution with
        | None -> 0L<tk>
        | Some s -> Solution.cost s
    )
    
#time
let result2 = part2 input
#time
