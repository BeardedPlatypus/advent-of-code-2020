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
        
    let rec isFinite (i: int) (acc: int) (instructionArray: (Instruction * bool)[]): int option =
        if i = Array.length instructionArray then
            Some acc
        else
            let inst, b = instructionArray.[i]
        
            if b then
                None
            else
                instructionArray.[i] <- (inst, true)
                
                match inst with
                | Nop _ -> isFinite (i + 1) acc instructionArray
                | Jmp j -> isFinite (i + j) acc instructionArray
                | Acc v -> isFinite (i + 1) (acc + v)  instructionArray
            

    let rec computeRec2 (i: int) (acc: int) (instructionArray: (Instruction * bool)[]) : int =
        if i = Array.length instructionArray then
            acc
        else
            let inst, _ = instructionArray.[i]
            instructionArray.[i] <- (inst, true)
            
            match inst with
            | Nop v ->
                // Act as if it were a Jmp
                match isFinite (i + v)  acc instructionArray with
                | Some res ->
                    res
                | None ->
                    computeRec2 (i + 1) acc instructionArray
            | Jmp v ->
                // Act as if it were a Nop
                match isFinite (i + 1)  acc instructionArray with
                | Some res ->
                    res
                | None ->
                    computeRec2 (i + v) acc instructionArray
            | Acc v ->
                computeRec2 (i + 1) (acc + v)  instructionArray
                
    let public calculateProblem2 () : int =
        let inp = Array.map (fun v -> (v, false)) inputValues
        computeRec2 0 0 inp

    // TODO: consolidate functions