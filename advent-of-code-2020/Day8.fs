namespace advent_of_code_2020

open System

module Day8 =
    type Instruction =
        | Nop of Int32
        | Acc of int32
        | Jmp of int32
        
    let toInstruction (inp: string): Instruction =
        let v = inp.Substring(4) |> Int32.Parse 
        match inp.Substring(0, 3) with
        | "acc" -> Acc v
        | "jmp" -> Jmp v
        | _     -> Nop v
        
    let inputValues : Instruction[] =
        System.IO.File.ReadAllText("resources/day8/input.txt").Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map toInstruction
        
    let rec computeRec1 (i: int) (acc: int) (instructionArray: (Instruction * bool)[]) : int =
        let inst, b = instructionArray.[i]
        
        if b then
            acc
        else
            instructionArray.[i] <- (inst, true)
            
            match inst with
            | Nop _ -> computeRec1 (i + 1) acc instructionArray
            | Jmp j -> computeRec1 (i + j) acc instructionArray
            | Acc v -> computeRec1 (i + 1) (acc + v)  instructionArray
    
    let public calculateProblem1 () : int =
        let inp = Array.map (fun v -> (v, false)) inputValues
        computeRec1 0 0 inp
