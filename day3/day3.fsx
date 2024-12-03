#r "nuget: FParsec"

open System
open System.IO

open FParsec
open FParsec.CharParsers

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

type Instruction =
    | Mul of int * int
    | Noop


let pMul =
    pipe5 (pstring "mul(") pint32 (pchar ',') pint32 (pchar ')')
        (fun _ x _ y _ -> Mul (x, y))

let pPart1 : Parser<_,unit> =
    many (choice [
        (attempt pMul)
        (skipAnyChar >>% Noop)
    ]) .>> eof

let part1 input =

    match run pPart1 input with
    | ParserResult.Success (xs, _, _) ->
        xs
        |> List.sumBy (fun x -> 
            match x with
            | Mul (a, b) -> (a * b)
            | Noop -> 0
        )
    | failure -> failwith $"%A{failure}"

let result1 = part1 input


type Instruction2 =
    | Mul of int * int
    | Noop
    | Do
    | Dont

let pMul2 =
    pipe5 (pstring "mul(") pint32 (pchar ',') pint32 (pchar ')')
        (fun _ x _ y _ -> Mul (x, y))

let pDo = pstring "do()" >>% Do
let pDont = pstring "don't()" >>% Dont

let pPart2 : Parser<_,unit> =
    many (choice [
        (attempt pMul2)
        pDo
        pDont
        (skipAnyChar >>% Noop)
    ]) .>> eof
    
let part2 input =
    match run pPart2 input with
    | ParserResult.Success (xs, _, _) ->
        ((0, true), xs)
        ||> List.fold (fun (sum, enabled) x -> 
            match x with
            | Do -> (sum, true)
            | Dont -> (sum, false)
            | Mul (a, b) -> 
                if enabled then 
                    (sum + (a * b), enabled)
                else 
                    (sum, enabled)
            | Noop -> (sum, enabled)
        )
    | failure -> failwith $"%A{failure}"

let result2 = part2 input