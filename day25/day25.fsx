#r "nuget: FParsec"

open System
open System.IO

let sample = """#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
"""

[<Struct>]
type Point = 
    {
        R: int
        C: int
    }

let colHeight (input: string[]) col char =
    input
    |> Seq.skip 1
    |> Seq.takeWhile (fun s -> s[col] = char)
    |> Seq.length

type Lock(input: string[]) = 
    let pinHeights = 
        Array.init (input[0].Length) (fun i ->
            colHeight input i '#'
        )
        
    member _.PinHeights = pinHeights

type Key(input: string[]) = 
    let bumpHeights = 
        Array.init (input[0].Length) (fun i ->
            5 - colHeight input i '.'
        )
        
    member _.BumpHeights = bumpHeights

type LockOrKey = 
    | L of Lock
    | K of Key

module LocksAndKeys = 
    open FParsec
    let pLockRow = pstring "#####" .>> newline
    let pKeyRow = pstring "....." .>> newline

    let pRow = (many1Chars (satisfyL (function | '.' | '#' -> true | _ -> false) "")) .>> newline

    let pLock =
        parse {
            let! a = pLockRow
            let! b = pRow
            let! c = pRow
            let! d = pRow
            let! e = pRow
            let! f = pRow
            let! g = pKeyRow
            return Lock [| a; b; c; d; e; f; g |] |> L
        }

    let pKey =
        parse {
            let! a = pKeyRow
            let! b = pRow
            let! c = pRow
            let! d = pRow
            let! e = pRow
            let! f = pRow
            let! g = pLockRow
            return Key [| a; b; c; d; e; f; g |] |> K
        }

    let pInput = (sepBy1 (choice [pLock; pKey]) newline) .>> eof

    let parse (input:string) = 
        match run pInput input with
        | Success (x, _, _) -> x
        | x -> failwith $"{x}"

let fits (lock: Lock) (key: Key) = 
    let rec f (pinHeights: int[]) (bumpHeights: int[]) i = 
        if i = pinHeights.Length then true
        else
            if pinHeights[i] + bumpHeights[i] > 5 then false
            else f pinHeights bumpHeights (i + 1)

    f lock.PinHeights key.BumpHeights 0

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let part1 input = 
    let locksAndKeys = LocksAndKeys.parse input
    let locks, keys = 
        (([], []), locksAndKeys) 
        ||> List.fold (fun (locks, keys) x ->
            match x with
            | L lock -> lock :: locks, keys
            | K key -> locks, key :: keys
        )
    
    seq {
        for l in locks do
            for k in keys do
                if fits l k then 1L
                else 0L
    }
    |> Seq.sum

#time
let result1 = part1 input
#time

printfn $"Part1: {result1}"
