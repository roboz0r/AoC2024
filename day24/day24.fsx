open System
open System.IO
open System.Collections.Immutable

type WireName = 
    | WireName of string
    member this.Name = 
        let (WireName name) = this
        name

type Wire = 
    {
        Name: WireName
        Value: bool //WireValue
    }

type GateKind =
    | AND
    | OR
    | XOR

type Gate =
    {
        Kind: GateKind
        Input1: WireName
        Input2: WireName
        Output: WireName
    }


let sample = """x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
"""

let sample2 = """x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
"""

let parse (input: string) = 
    let wires = ImmutableArray.CreateBuilder()
    let gates = ImmutableArray.CreateBuilder()
    for line in input.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) do
        match line.IndexOf(':') with
        | -1 ->
            match line.Split(' ') with
            | [| w1; g; w2; _;  wo |] ->
                let gate = 
                    match g with
                    | "AND" -> AND
                    | "OR" -> OR
                    | "XOR" -> XOR
                    | _ -> failwith $"Unexpected {g}"

                {
                    Kind = gate
                    Input1 = WireName w1
                    Input2 = WireName w2
                    Output = WireName wo
                } |> gates.Add
            | _ -> failwith $"Unexpected gate {line}"
        | i ->
            let name = WireName (line[0 .. i - 1])
            let state = 
                match line[line.Length - 1] with
                | '0' -> false
                | '1' -> true
                | _ -> failwith $"Unexpected wire {line}"
            {
                Name = name
                Value = state
            } |> wires.Add
            
    wires.ToImmutable(), gates.ToImmutable()

let xor a b =
    match a, b with
    | true, true 
    | false, false -> false
    | _ -> true

let rec run (wires: Map<WireName, bool>) (gates: ImmutableArray<Gate>) =
    if gates.Length = 0 then wires
    else
        ((wires, ImmutableArray.CreateBuilder()), gates)
        ||> Seq.fold (fun (wires, remainingGates) g ->
            match wires |> Map.tryFind g.Input1, wires |> Map.tryFind g.Input2 with
            | Some v1, Some v2 ->
                let vo = 
                    match g.Kind with
                    | AND -> v1 && v2
                    | OR -> v1 || v2
                    | XOR -> xor v1 v2
                let wires = wires |> Map.add g.Output vo
                wires, remainingGates
            | _ ->
                remainingGates.Add g
                wires, remainingGates
        )
        |> fun (wires, gates) -> run wires (gates.ToImmutable())

let part1 input = 
    let wires, gates = parse input
    let wires = 
        wires 
        |> Seq.map (fun w ->
            w.Name, w.Value
        )
        |> Map
    
    let wires = run wires gates
    wires
    |> Seq.choose (fun (KeyValue(name, value)) ->
        let (WireName name) = name
        if name.StartsWith('z') then
            if value then 
                let i = int (name[1..2])
                Some (1L <<< i)
            else
                Some 0L
        else
            None
    )
    |> Seq.reduce (|||)

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

#time
let result1 = part1 input
#time

let createWires prefix value = 
    Array.init 45 (fun i ->
        (WireName $"%c{prefix}%02i{i}", 
            match (value >>> i) &&& 1L with
            | 0L -> false
            | _ -> true)
    )

let createWiresXY x y = 
    [|
        createWires 'x' x
        createWires 'y' y
    |]
    |> Seq.concat
    |> Map

let swapOutput g1 g2 = 
    { g1 with Output = g2.Output }, { g2 with Output = g1.Output }

let printSingleBitErrors () =
    // Found errors around bits 14, 18, 23, 34
    let simulate input = 
        let _, gates = parse input
        fun x y ->
            let wires = createWiresXY x y
            let wires = run wires gates
            wires
            |> Seq.choose (fun (KeyValue(name, value)) ->
                let (WireName name) = name
                if name.StartsWith('z') then
                    if value then 
                        let i = int (name[1..2])
                        Some (1L <<< i)
                    else
                        Some 0L
                else
                    None
            )
            |> Seq.reduce (|||)

    let runPart2 = 
        let simulate = simulate input
        fun x y ->
            let z = simulate x y
            if x + y <> z then
                printfn "X:%i %f\nY:%i %f" x (Math.Log2 (float x)) y (Math.Log2 (float y))
                printfn "X:%045B\nY:%045B\nZ:%045B\n\n" x y z

    for xi in 0 .. 44 do
        runPart2 (1L <<< xi) 0L

    for yi in 0 .. 44 do
        runPart2 0L (1L <<< yi)

    for xi in 0 .. 44 do
        runPart2 (1L <<< xi) (1L <<< xi)


let renderMermaidNodes () = 
    let name gate = 
        if gate.Input1.Name < gate.Input2.Name then
            $"{gate.Input1.Name}{gate.Kind}{gate.Input2.Name}"
        else
            $"{gate.Input2.Name}{gate.Kind}{gate.Input1.Name}"
    let _, gates = parse input

    let onOutput gate = 
        gates
        |> Seq.filter (fun g ->
            let name  = gate.Output
            if g.Input1 = name || g.Input2 = name then
                true
            else false
        )

    gates
    |> Seq.map (fun g -> g, onOutput g)
    |> Seq.collect (fun (g, gs) ->
        // Render Mermaid format
        let output = 
            if Seq.isEmpty gs then Seq.singleton g.Output.Name
            else gs |> Seq.map name
        output
        |> Seq.map (fun output ->
            sprintf "    %s -->|%s| %s" (name g) g.Output.Name output)
        )
    |> Seq.sortBy (fun s -> s[(s.Length - 4)..])
    |> Seq.iter (printfn "%s")

// renderMermaidNodes()

let part2 input = 
    let wires, gates = parse input
    let wires = 
        wires 
        |> Seq.map (fun w ->
            w.Name, w.Value
        )
        |> Map

    // Found by analysis of mermaid diagram
    let makeGate i1 kind i2 o = 
        { Input1 = WireName i1; Kind = kind; Input2 = WireName i2; Output = WireName o }
    let remove14_1 = makeGate "sjr" OR "tck" "z14"
    let remove14_2 = makeGate "dfb" XOR "bfn" "hbk"
    let add14_1, add14_2 = swapOutput remove14_1 remove14_2
    
    let remove18_1 = makeGate "y18" AND "x18" "z18"
    let remove18_2 = makeGate "grp" XOR "fgr" "kvn"
    let add18_1, add18_2 = swapOutput remove18_1 remove18_2

    
    let remove23_1 = makeGate "dvw" AND "rpg" "z23"
    let remove23_2 = makeGate "dvw" XOR "rpg" "dbb"
    let add23_1, add23_2 = swapOutput remove23_1 remove23_2

    
    let remove34_1 = makeGate "y34" AND "x34" "cvh"
    let remove34_2 = makeGate "x34" XOR "y34" "tfn"
    let add34_1, add34_2 = swapOutput remove34_1 remove34_2

    let toAdd =
        [|
            add14_1
            add14_2
            add18_1
            add18_2
            add23_1
            add23_2
            add34_1
            add34_2
        |]

    let gatesRem = gates.RemoveAll(fun g ->
        g = remove14_1
        || g = remove14_2
        || g = remove18_1
        || g = remove18_2
        || g = remove23_1
        || g = remove23_2
        || g = remove34_1
        || g = remove34_2
    )
    if gates.Length - 8 <> gatesRem.Length then 
        failwith "Didn't remove all expected gates"

    let gates = gatesRem.AddRange toAdd

    let simulate gates x y =
        let wires = createWiresXY x y
        let wires = run wires gates
        wires
        |> Seq.choose (fun (KeyValue(name, value)) ->
            let (WireName name) = name
            if name.StartsWith('z') then
                if value then 
                    let i = int (name[1..2])
                    Some (1L <<< i)
                else
                    Some 0L
            else
                None
        )
        |> Seq.reduce (|||)

    let failOnErrors x y = 
        let simulate = simulate gates
        let z = simulate x y
        if x + y <> z then
            printfn "X:%i %f\nY:%i %f" x (Math.Log2 (float x)) y (Math.Log2 (float y))
            printfn "X:%045B\nY:%045B\nZ:%045B\n\n" x y z
            failwithf "Unexpected output"

    for xi in 0 .. 44 do
        failOnErrors (1L <<< xi) 0L

    for yi in 0 .. 44 do
        failOnErrors 0L (1L <<< yi)

    for xi in 0 .. 44 do
        failOnErrors (1L <<< xi) (1L <<< xi)

    toAdd
    |> Array.map _.Output.Name
    |> Array.sort
    |> fun xs -> String.Join(',', xs)

#time
let result2 = part2 input
#time

printfn $"Part1: {result1} Part2: {result2}"
