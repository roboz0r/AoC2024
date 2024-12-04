open System
open System.IO

let sample = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

type XMASInput(input: string array) =
    member _.Item with get (r, c) = input[r][c]
    member _.Item with get (r) = input[r]

    member _.Width = input[0].Length
    member _.Height = input.Length

let parse (input: string) =
    input.Split([| '\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> XMASInput

[<Struct>]
type Word =
    {
        Start: struct (int*int)
        End: struct (int*int)
    }
    static member Create (r1, c1, r2, c2) =
        {
            Start = struct (r1, c1)
            End = struct (r2, c2)
        }

let rec findChar char (input: XMASInput) r c =
    if char = input[r,c] then
        Some (r, c)
    else
        if c < input.Width - 1 then
            findChar char input r (c + 1)
        else
            let c = 0
            if r < input.Height - 1 then 
                findChar char input (r + 1) c
            else
                None

let findX (input: XMASInput) r c =
    findChar 'X' input r c

let l2r (input: XMASInput) r c =
    if c + 3 < input.Width then 
        if input[r].AsSpan(c+1, 3).SequenceEqual("MAS") then 
            Some (Word.Create(r, c, r, c + 3))
        else None
    else None

let r2l (input: XMASInput) r c =
    if c - 3 >= 0 then 
        if input[r].AsSpan(c - 3, 3).SequenceEqual("SAM") then 
            Some (Word.Create(r, c, r, c - 3))
        else None
    else None

let downRight (input: XMASInput) r c =
    if r + 3 < input.Height && c + 3 < input.Width then 
        match input[r+1,c+1], input[r+2,c+2], input[r+3,c+3] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r + 3, c + 3))
        | _ -> None
    else None

let t2b (input: XMASInput) r c =
    if r + 3 < input.Height then 
        match input[r+1,c], input[r+2,c], input[r+3,c] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r + 3, c))
        | _ -> None
    else None

let downLeft (input: XMASInput) r c =
    if r + 3 < input.Height && c - 3 >= 0 then 
        match input[r+1,c-1], input[r+2,c-2], input[r+3,c-3] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r + 3, c - 3))
        | _ -> None
    else None

let upLeft (input: XMASInput) r c =
    if r - 3 >= 0 && c - 3 >= 0 then 
        match input[r-1,c-1], input[r-2,c-2], input[r-3,c-3] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r - 3, c - 3))
        | _ -> None
    else None

let b2t (input: XMASInput) r c =
    if r - 3 >= 0 then 
        match input[r-1,c], input[r-2,c], input[r-3,c] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r - 3, c))
        | _ -> None
    else None

let upRight (input: XMASInput) r c =
    if r - 3 >= 0 && c + 3 < input.Width then 
        match input[r-1,c+1], input[r-2,c+2], input[r-3,c+3] with
        | 'M', 'A', 'S' -> Some (Word.Create(r, c, r - 3, c + 3))
        | _ -> None
    else None

let directions = 
    [|
        l2r
        downRight
        t2b
        downLeft
        r2l
        upLeft
        b2t
        upRight
    |] 

let rec findXMAS (input: XMASInput) r c =
    match findX input r c with
    | Some (r, c) ->
        directions
        |> Array.choose (fun f -> f input r c)
        |> function
        | [| |] -> 
            if c < input.Width - 1 then
                findXMAS input r (c + 1)
            else
                let c = 0
                if r < input.Height - 1 then 
                    findXMAS input (r + 1) c
                else
                    None
        | xs -> Some xs
    | None -> 
        None

let findXMASCount input =
    let rec f count input r c =
        match findXMAS input r c with
        | Some word ->
            let count = count + word.Length
            let struct (r, c) = word[0].Start
            if c < input.Width - 1 then
                f count input r (c + 1)
            else
                let c = 0
                if r < input.Height - 1 then 
                    f count input (r + 1) c
                else
                    count
        | None -> count

    f 0 input 0 0

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let result1 = 
    input
    |> parse
    |> findXMASCount

let findA input r c = findChar 'A' input r c 

let rec findXMAS2 (input: XMASInput) r c =
    match findA input r c with
    | Some (r, c) ->
        if r >= 1 
            && r < input.Width - 1
            && c >= 1 
            && c < input.Width - 1 then 
                match input[r-1,c-1], input[r+1,c+1], input[r+1,c-1], input[r-1,c+1] with
                | 'M', 'S', 'M', 'S'
                | 'M', 'S', 'S', 'M'
                | 'S', 'M', 'M', 'S'
                | 'S', 'M', 'S', 'M' -> Some (r, c)
                | _ ->
                    if c < input.Width - 2 then
                        findXMAS2 input r (c + 1)
                    else
                        let c = 1
                        if r < input.Height - 2 then 
                            findXMAS2 input (r + 1) c
                        else
                            None
        else 
            if c < input.Width - 2 then
                findXMAS2 input r (c + 1)
            else
                let c = 1
                if r < input.Height - 2 then 
                    findXMAS2 input (r + 1) c
                else
                    None
    | None -> 
        None

let findXMASCount2 input =
    let rec f count input r c =
        match findXMAS2 input r c with
        | Some word ->
            let count = count + 1
            let (r, c) = word
            if c < input.Width - 2 then
                f count input r (c + 1)
            else
                let c = 1
                if r < input.Height - 2 then 
                    f count input (r + 1) c
                else
                    count
        | None -> count

    f 0 input 1 1

let result2 = 
    input
    |> parse
    |> findXMASCount2