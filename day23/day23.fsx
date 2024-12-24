open System
open System.IO
open System.Collections.Generic
open System.Collections.Frozen

type ComputerId = string

[<CustomComparison; CustomEquality>]
type Pair = 
    | Pair of a:ComputerId * b:ComputerId
    interface IEquatable<Pair> with 
        member this.Equals(other: Pair) =
            match this, other with
            | Pair(a1, b1), Pair(a2, b2) ->
                (a1 = a2 && b1 = b2)
                || (a1 = b2 && b1 = a2)

    override this.Equals (obj: obj): bool = 
            match obj with
            | :? Pair as p -> (this :> IEquatable<Pair>).Equals(p)
            | _ -> false

    override this.GetHashCode (): int = 
            let (Pair(a, b)) = this
            a.GetHashCode() ^^^ b.GetHashCode()

    interface IComparable with        
        member this.CompareTo(obj: obj): int = 
            match obj with
            | :? Pair as p -> 
                let (Pair(a1, b1), Pair(a2, b2)) = this, p
                let arr1 = [|a1;b1|]
                let arr2 = [|a2;b2|]
                Array.sortInPlace arr1
                Array.sortInPlace arr2
                Array.compareWith (fun a b -> StringComparer.Ordinal.Compare(a, b)) arr1 arr2

            | _ -> invalidOp $"cannot compare Pair to {obj.GetType()}"

    member this.Contains(id) = 
        let (Pair(a, b)) = this
        a = id || b = id


    member this.StartsWith(c: char) = 
        let (Pair(a, b)) = this
        a.StartsWith(c) || b.StartsWith(c)

    interface IEnumerable<ComputerId> with
        member this.GetEnumerator (): IEnumerator<ComputerId> = 
                (seq {
                    let (Pair(a, b)) = this
                    a
                    b
                }).GetEnumerator()
                
        member this.GetEnumerator (): Collections.IEnumerator = 
                (this :> IEnumerable<ComputerId>).GetEnumerator()


let sample = """kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
"""

let parse (input: string) = 
    input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        match s.Split('-') with
        | [| a; b|] -> Pair(a.Trim(), b.Trim())
        | _ -> failwith $"Unexpected input {s}"
    )

parse sample

let index pairs = 
    pairs
    |> Seq.collect (fun (Pair(a, b) as p) ->
        [|
            a, p
            b, p
        |]
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (id, xs) ->
        id, xs |> Seq.map snd |> Array.ofSeq
    )
    |> dict
    |> FrozenDictionary.ToFrozenDictionary

let sampleIndex = 
    sample
    |> parse
    |> index

sampleIndex["co"] |> Array.contains (Pair ("co", "ta"))

let permutationsBag choices =
    seq {
        let numChoices = Array.length choices
        for i in 0..(numChoices - 2) do
            for j in (i + 1) .. (numChoices - 1) do
            yield choices[i], choices[j]
    }

permutationsBag [|1;2;3;4|]
|> List.ofSeq

let other x (Pair(a, b)) = 
    if x = a then b
    elif x = b then a
    else invalidOp $"{x} not in {a} {b}"

let findTriples (index: FrozenDictionary<ComputerId, Pair[]>) = 
    seq {
        for (KeyValue(id, pairs)) in index do
            permutationsBag pairs
            |> Seq.choose (fun (p1, p2) ->
                let b1 = other id p1
                let b2 = other id p2
                let p3 = Pair(b1, b2)
                index[b1]
                |> Array.tryPick (fun px ->
                    if px = p3 then 
                        Some (Set [p1; p2; p3])
                    else 
                        None
                )
            )
    }
    |> Seq.concat
    |> Set

let anyStartsWith (c: char) pairs =
    pairs
    |> Seq.tryFind (fun (p: Pair) -> p.StartsWith(c))

let part1 input = 
    input
    |> parse
    |> index
    |> findTriples
    |> Seq.choose (anyStartsWith 't')
    |> Seq.length

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
// ∪ Union is the set of all objects that are a member of A, or B, or both
// ∩ Intersection is the set of all objects that are members of both A and B
//  \ Set difference  U \ A, is the set of all members of U that are not members of A

// algorithm BronKerbosch1(R, P, X) is
//     if P and X are both empty then
//         report R as a maximal clique
//     for each vertex v in P do
//         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
//         P := P \ {v}
//         X := X ⋃ {v}

let getNeighbours (index: FrozenDictionary<ComputerId, Pair[]>) v = 
    index[v] |> Array.map (other v) |> Set

let rec bronKerbosch getNeighbours r p x  =
    seq {
        if Set.isEmpty p && Set.isEmpty x then 
            yield r // Report Maximal Clique
        else
            let mutable p2 = p
            let mutable x2 = x
            for v in p do
                let neighbours = getNeighbours v
                yield! bronKerbosch getNeighbours (r |> Set.add v) (Set.intersect p2 neighbours) (Set.intersect x2 neighbours)
                p2 <- p2 |> Set.remove v
                x2 <- x2 |> Set.add v
    }

let bronKerbosch2 getNeighbours vertices =
    let rec f getNeighbours r p x  =
        if Set.isEmpty p && Set.isEmpty x then 
            Seq.singleton r // Report Maximal Clique
        else
            ((p, x, Seq.empty), p)
            ||> Seq.fold (fun (p, x, acc) v ->
                let neighbours = getNeighbours v
                let acc = Seq.concat [| acc; (f getNeighbours (r |> Set.add v) (Set.intersect p neighbours) (Set.intersect x neighbours)) |]
                (p |> Set.remove v, x |> Set.add v, acc)
            )
            |> fun (_, _, acc) -> acc
            
    f getNeighbours Set.empty vertices Set.empty

let part2 input = 
    let inputIndex = 
        input
        |> parse
        |> index
    let vertices = inputIndex.Keys |> Set
    bronKerbosch2 (getNeighbours inputIndex) vertices
    |> Seq.maxBy _.Count
    |> (fun longestSet -> String.Join(',', longestSet))

#time
let result2 = part2 input
#time

printfn $"Part1: {result1} Part2: {result2}"
