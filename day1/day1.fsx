open System
open System.IO

let sample = """3   4
4   3
2   5
1   3
3   9
3   3"""

let part1 (sample: string) = 

    sample.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> 
        match line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| x; y |] -> (int x, int y)
        | _ -> failwith "Invalid input"
    )
    |> Array.unzip
    |> fun (xs, ys) -> (Array.sort xs, Array.sort ys)
    |> fun (xs, ys) -> (xs, ys) ||> Array.map2 (fun x y -> abs(y - x))
    |> Array.sum


let input1 = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input1.txt"))

let result1 = part1 input1

let part2 (sample: string) = 

    sample.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> 
        match line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| x; y |] -> (int x, int y)
        | _ -> failwith "Invalid input"
    )
    |> Array.unzip
    |> fun (xs, ys) -> (Set xs, ys |> Array.countBy id |> Map)
    |> fun (left, right) ->
        left
        |> Seq.sumBy (fun i ->
            right
            |> Map.tryFind i
            |> function
            | Some n -> i * n
            | None -> 0)
        
let result2 = part2 input1

