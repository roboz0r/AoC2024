#r "nuget: FParsec"

open System
open System.IO
open System.Text

let sample = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""

type Instruction =
    | ADV = 0
    | BXL = 1
    | BST = 2
    | JNZ = 3
    | BXC = 4
    | OUT = 5
    | BDV = 6
    | CDV = 7

type Combo =
    | Literal0 = 0 // Combo operands 0 through 3 represent literal values 0 through 3.
    | Literal1 = 1
    | Literal2 = 2
    | Literal3 = 3
    | RegisterA = 4 // Combo operand 4 represents the value of register A.
    | RegisterB = 5 // Combo operand 5 represents the value of register B.
    | RegisterC = 6 // Combo operand 6 represents the value of register C.
    | Reserved_7 = 7 // Combo operand 7 is reserved and will not appear in valid programs.

[<Struct>]
type Registers = 
    {
        A: int64
        B: int64
        C: int64
        // InstructionPtr: int // the instruction pointer increases by 2 after each instruction is processed
    }


type Program = int[]

type InitialState = 
    {
        Registers: Registers
        Program: Program
    }

module InitialState =
    open FParsec
    let pRegister name = 
        pipe5 (pstring "Register ") (pchar name) (pstring ": ") pint64 newline (fun _ _ _ x _ -> x)
    let pA = pRegister 'A'
    let pB = pRegister 'B'
    let pC = pRegister 'C'

    let pProgram = pipe3 (pstring "Program: ") (sepBy1 pint32 (pchar ',')) newline (fun _ xs _ -> xs |> Array.ofList)

    let pState = pipe5 pA pB pC newline pProgram (fun a b c _ p -> 
        {
            Registers = { A = a; B = b; C = c }
            Program = p
        })
    
    let parse (input: string) =
        match run (pState .>> eof) input with
        | Success (s, _, _) -> s
        | Failure (s, err, _) -> failwithf "%s %A" s err

type Output() =
    let sb = StringBuilder()
    member _.Append(num: int64) =
        if sb.Length = 0 then
            sb.Append(num) |> ignore
        else
            sb.Append(',').Append(num) |> ignore

    member _.Clear() = sb.Clear() |> ignore

    override _.ToString (): string = 
            sb.ToString()

let comboValue (r: Registers) op =
    match op with
    | Combo.Literal0 -> 0L
    | Combo.Literal1 -> 1L
    | Combo.Literal2 -> 2L
    | Combo.Literal3 -> 3L
    | Combo.RegisterA -> r.A
    | Combo.RegisterB -> r.B
    | Combo.RegisterC -> r.C
    | Combo.Reserved_7
    | _ -> invalidOp $"Unexpected combo operand {op}"

let rec run (program: Program) (out: Output) (r: Registers) (instPtr: int) =
    if instPtr + 1 >= program.Length then (r, out.ToString())
    else
        let run = run program out
        let inst = enum<Instruction>program[instPtr]
        let op = enum<Combo>program[instPtr + 1]
        // printfn $"A: {r.A}, B: {r.B}, C: {r.C}\n{inst} {op}"
        match inst with
        // The numerator is the value in the A register. The denominator is found by raising 2 
        // to the power of the instruction's combo operand. 
        // (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
        // The result of the division operation is truncated to an integer and then written to the A register.
        | Instruction.ADV ->
            let num = r.A
            let denom = pown 2L (int (comboValue r op))
            let r = { r with A = num / denom }
            run r (instPtr + 2)
        // Bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
        | Instruction.BXL ->
            let r = { r with B = r.B ^^^ (int64 op) }
            run r (instPtr + 2) 
        // calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), 
        // then writes that value to the B register.
        | Instruction.BST ->
            let r = { r with B = (comboValue r op) % 8L }
            run r (instPtr + 2) 
        // does nothing if the A register is 0. However, if the A register is not zero, it jumps by 
        // setting the instruction pointer to the value of its literal operand; if this instruction jumps, 
        // the instruction pointer is not increased by 2 after this instruction.
        | Instruction.JNZ ->
            match r.A with
            | 0L -> run r (instPtr + 2) 
            | a -> run r (int op)

        // calculates the bitwise XOR of register B and register C, then stores the result in register B. 
        // (For legacy reasons, this instruction reads an operand but ignores it.)
        | Instruction.BXC ->
            let r = { r with B = r.B ^^^ r.C }
            run r (instPtr + 2) 
        // calculates the value of its combo operand modulo 8, then outputs that value. 
        // (If a program outputs multiple values, they are separated by commas.)
        | Instruction.OUT ->
            let x = comboValue r op
            out.Append(x % 8L)
            run r (instPtr + 2) 
        // works exactly like the adv instruction except that the result is stored in the B register. 
        // (The numerator is still read from the A register.)
        | Instruction.BDV ->
            let num = r.A
            let denom = pown 2L (int (comboValue r op))
            let r = { r with B = num / denom }
            run r (instPtr + 2)
        // works exactly like the adv instruction except that the result is stored in the C register. 
        // (The numerator is still read from the A register.)
        | Instruction.CDV ->
            let num = r.A
            let denom = pown 2L (int (comboValue r op))
            let r = { r with C = num / denom }
            run r (instPtr + 2)
        | _ -> invalidOp $"Unexpected instruction {inst}"

let reg a b c = { A = a; B = b; C = c }

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let part1 (input: string) = 
    let x = InitialState.parse input
    run x.Program (Output()) x.Registers 0

#time
let (reg1, result1) = part1 input
#time

let rec findQuine (expected: string) output p =
    let findPartial r = 
        seq {
            for i in 0L .. 7L do
                let rI = { r with A = r.A + i }
                let (_, o) = run p output rI 0
                output.Clear()
                if expected.EndsWith(o) then
                    rI, expected = o
        }

    let rec findSolutions r =
        seq {
            for (r, success) in findPartial r do
                if success then r.A
                else
                    yield! findSolutions { r with A = 8L * r.A }
        }

    findSolutions (reg 0L 0L 0L)
    
let part2 (input: string) = 
    let x = InitialState.parse input
    let expected = String.Join(',', x.Program)
    findQuine expected (Output()) x.Program
    |> Seq.head 

#time
let result2 = part2 input
#time
printfn $"Part1: {result1} Part2: {result2}"
