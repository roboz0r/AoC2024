open System
open System.IO

let sample = "2333133121414131402"


let digitVal (c: char) = 
    int c - 48

[<Literal>]
let Empty = -1

let diskLength (input: string) = 
    let rec f (input: string) i sum =
        if i = input.Length then sum
        else
            f input (i + 1) (sum + (digitVal (input[i])))

    f input 0 0

diskLength sample

let init (input: string) =
    let rec f iIn iOut arr =
        if iIn = input.Length then arr
        else
            let count = digitVal (input[iIn])
            match iIn % 2 with
            | 0 ->
                Array.fill arr iOut count (iIn / 2)
            | _ -> 
                Array.fill arr iOut count Empty
            f (iIn + 1) (iOut + count) arr

    let len = diskLength input
    f 0 0 (Array.zeroCreate len)


init sample
|> printfn "%A"


let compact (arr: int array) =
    let rec findEmpty iStart iEnd (arr: int array) =
        if iStart >= iEnd then None
        else
            match arr[iStart] with
            | Empty -> Some iStart
            | _ -> findEmpty (iStart + 1) iEnd arr

    let rec f iStart iEnd (arr: int array) =
        match arr[iEnd] with
        | Empty -> f iStart (iEnd - 1) arr
        | x -> 
            match findEmpty iStart iEnd arr with
            | Some target ->
                arr[target] <- x
                f (target + 1) (iEnd - 1) arr
            | None -> iEnd, arr

    f 0 (arr.Length - 1) arr

let checksum iEnd (arr: int array)  = 
    let rec f (arr: int array) iEnd i sum = 
        if i = iEnd then (sum + int64(i * arr[i]))
        else
            f arr iEnd (i + 1) (sum + int64(i * arr[i]))
    f arr iEnd 0 0L

let part1 (input: string) = 
    let input = input.TrimEnd([|'\r'; '\n'|])
    input
    |> init
    |> compact
    ||> checksum

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

let compact2 (input: string) (arr: int array) =
    let rec findEmpty iStart iEnd len =
        if iStart + len >= iEnd then None
        else
            match arr[iStart] with
            | Empty -> 
                let span = arr.AsSpan(iStart, len)
                if span.ContainsAnyExcept(Empty) then 
                    findEmpty (iStart + 1) iEnd len
                else
                    Some iStart
            | _ -> findEmpty (iStart + 1) iEnd len

    let rec f iStart iEnd iInput =
        if iInput < 0 || iEnd < iStart then ()
        else
            let len = digitVal input[iInput]
            match iInput % 2 with
            | 0 ->
                match findEmpty iStart iEnd len with
                | None ->
                    f iStart (iEnd - len) (iInput - 1)

                | Some target ->
                    Array.fill arr target len (iInput / 2)
                    Array.fill arr (iEnd - len) len Empty

                    let iStart = 
                        if iStart = target then 
                            iStart + len
                        else
                            iStart

                    f iStart (iEnd - len) (iInput - 1)

            | _ ->
                // Free space fragment
                f iStart (iEnd - len) (iInput - 1)

    f 0 (arr.Length) (input.Length - 1)
    arr

let checksum2 (arr: int array) = 
    printfn "%A" arr
    let rec f (arr: int array) i sum = 
        if i = Array.length arr then sum
        else
            match arr[i] with
            | Empty -> f arr (i + 1) sum
            | x -> 
                f arr (i + 1) (sum + int64(i * x))
    f arr 0 0L


let part2 (input: string) = 
    let input = input.TrimEnd([|'\r'; '\n'|])
    input
    |> init
    |> compact2 input
    |> checksum2

#time
let result2 = part2 input
#time