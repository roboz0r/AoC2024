open System
open System.IO
open System.Collections.Immutable

type PageOrder =
    {
        Before: int
        After: int
    }


type PageCollection = ImmutableArray<int>
type PageCollections = ImmutableArray<PageCollection>

let sample = 
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

let parse (input:string) =
    let ordering = ImmutableArray.CreateBuilder()
    let collections = ImmutableArray.CreateBuilder()
    input.Split([|'\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.iter ( fun x ->
        match x.IndexOf('|'), x.IndexOf(',') with
        | i, -1 ->
            ordering.Add({Before = Int32.Parse(x.AsSpan(0, i)); After = Int32.Parse(x.AsSpan(i + 1))})
        | -1, i ->
            collections.Add(x.Split([|',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> _.ToImmutableArray())
        | _ -> failwith $"Invalid input {x}"
    )

    ordering.ToImmutable(), collections.ToImmutable()

let parsedSample = parse sample

type PageOrdering =
    {
        Befores: Map<int, Set<int>>
        Afters: Map<int, Set<int>>
    }

let makeOrdering (xs: ImmutableArray<PageOrder>) =
    let addOrUpdate ordering order =
        let newBefores = 
            match ordering.Befores |> Map.tryFind order.Before with
            | None -> ordering.Befores |> Map.add order.Before (Set.singleton order.After)
            | Some befores -> ordering.Befores |> Map.add order.Before (befores |> Set.add order.After)
        let newAfters =
            match ordering.Afters |> Map.tryFind order.After with
            | None -> ordering.Afters |> Map.add order.After (Set.singleton order.Before)
            | Some afters -> ordering.Afters |> Map.add order.After (afters |> Set.add order.Before)
        {
            Befores = newBefores
            Afters = newAfters
        }

    let rec f ordering i =
        if i = xs.Length then ordering
        else
            let order = xs[i]
            let ordering = addOrUpdate ordering order
            f ordering (i + 1)

    f { Befores = Map.empty; Afters = Map.empty } 0

let sampleOrdering = makeOrdering (fst (parse sample))

let middle (pages: ImmutableArray<int>) =
    let len = pages.Length
    match len % 2 with
    | 1 -> pages[(len / 2)]
    | _ -> failwith $"Expected odd number of pages {String.Join(',', pages)}"

let rec beforeOk p (span: ReadOnlySpan<int>) (pageOrdering: PageOrdering) i =
    if i = span.Length then true
    else
        let toCheck = span[i]
        match pageOrdering.Befores.TryFind(p) with
        | Some set -> 
            if set.Contains(toCheck) then false
            else
                beforeOk p span pageOrdering (i + 1)
        | None -> 
            beforeOk p span pageOrdering (i + 1)

let rec afterOk p (span: ReadOnlySpan<int>) (pageOrdering: PageOrdering) i =
    if i = span.Length then true
    else
        let toCheck = span[i]
        match pageOrdering.Afters.TryFind(p) with
        | Some set -> 
            if set.Contains(toCheck) then false
            else
                afterOk p span pageOrdering (i + 1)
        | None -> 
            afterOk p span pageOrdering (i + 1)

let rec isSortedSpan (pageOrdering: PageOrdering) (pages: ReadOnlySpan<int>) i =
    if i = pages.Length then true
    else 
        let p = pages[i]
        let before =
            if i = 0 then ReadOnlySpan.Empty
            else
                pages.Slice(0, i - 1)
        let after = 
            if i = pages.Length - 1 then ReadOnlySpan.Empty
            else
                pages.Slice(i + 1)
        if (beforeOk p before pageOrdering 0) && (afterOk p after pageOrdering 0) then 
            isSortedSpan pageOrdering pages (i + 1)
        else false

let isSorted (pageOrdering: PageOrdering) (pages: ImmutableArray<int>) =
    isSortedSpan pageOrdering (pages.AsSpan()) 0

parsedSample
|> snd
|> Array.ofSeq
|> Array.filter (isSorted sampleOrdering)
|> Array.sumBy middle

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
let parsedInput = parse input

let result1 = 
    parsedInput
    |> (fun (order, pages) ->
        let ordering = makeOrdering order
        pages
        |> Array.ofSeq
        |> Array.filter (isSorted ordering)
        |> Array.sumBy middle
    )

let ro x:ReadOnlySpan<_> = Span.op_Implicit x

let rec insertSorted (pageOrdering: PageOrdering) (sortedPages: ReadOnlySpan<int>) (workingBuffer: int array) toInsert tryInsertAt =
    let toCheck = (ro (workingBuffer.AsSpan(0, sortedPages.Length + 1)))

    // printfn "%s" (String.Join(',', sortedPages.ToArray()))
    if tryInsertAt = sortedPages.Length then 
        sortedPages.CopyTo(workingBuffer)
        workingBuffer[tryInsertAt] <- toInsert
        // Didn't find a sort so put it at the end
        // printfn "Check after end insert %s" (String.Join(',', toCheck.ToArray()))
        if isSortedSpan pageOrdering toCheck 0 then 
            true
        else
            false
    else
        for j in 0 .. sortedPages.Length do
            if j = tryInsertAt then 
                workingBuffer[j] <- toInsert
            elif j < tryInsertAt then 
                workingBuffer[j] <- sortedPages[j]
            else
                workingBuffer[j] <- sortedPages[j - 1]
    
        // printfn "Check after loop insert %s" (String.Join(',', toCheck.ToArray()))

        if isSortedSpan pageOrdering toCheck 0 then 
            true
        else
            insertSorted pageOrdering sortedPages workingBuffer toInsert (tryInsertAt + 1)


let findOrder (pageOrdering: PageOrdering) (pages: ImmutableArray<int>) =
    let sortedBuffer = Array.zeroCreate pages.Length
    let workingBuffer = Array.zeroCreate pages.Length
    
    let rec f i = 
        if i = pages.Length then 
            workingBuffer.ToImmutableArray()
        elif insertSorted pageOrdering (ro (sortedBuffer.AsSpan(0, i))) workingBuffer (pages[i]) 0 then
            workingBuffer.AsSpan(0, i + 1).CopyTo(sortedBuffer)
            f (i + 1)
        else
            failwith $"Unable to sort {String.Join(',', pages)}"

    f 0

let result2 = 
    let ordering, notSorted =
        parsedInput
        |> (fun (order, pages) ->
            let ordering = makeOrdering order
            ordering, 
                pages
                |> Array.ofSeq
                |> Array.filter (isSorted ordering >> not)
        )

    notSorted
    |> Array.map (findOrder ordering)
    |> Array.sumBy middle

