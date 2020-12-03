namespace advent_of_code_2020

module Day3 =
    type Field =
        | Tree
        | Open
        
    let toField (c: char): Field =
        if c = '.' then Open else Tree
        
    let inputValues () : Field[] List =
        System.IO.File.ReadLines("resources/day3/input.txt")
        |> Seq.map (fun s -> Seq.map toField s |> Array.ofSeq)
        |> List.ofSeq
        
    let compute (xOffset: int) ((curPos: int), (count: int)) (line: Field[]): int * int =
        let nextPos = (curPos + xOffset) % line.Length
        match line.[curPos] with
        | Tree -> (nextPos, count + 1)
        | Open -> (nextPos, count)
    
    let calculate (xOffset: int) (input: Field[] List): int =
        let _, res = List.fold (compute xOffset) (xOffset, 0) input.Tail
        res
        
    let public calculateProblem1 (): int =
        let inputValues = inputValues ()
        calculate 3 inputValues
        
    let reduceWithYOffset (yOffset: int) (data: Field[] List): Field[] List =
        List.mapi (fun (i: int) v -> if (i % yOffset) = 0 then Some v else None ) data
        |> List.choose id
        
    let public calculateProblem2 (): int =
        let inputValues = inputValues ()
        let val1 = calculate 1 (inputValues |> reduceWithYOffset 1)
        let val2 = calculate 3 (inputValues |> reduceWithYOffset 1)
        let val3 = calculate 5 (inputValues |> reduceWithYOffset 1)
        let val4 = calculate 7 (inputValues |> reduceWithYOffset 1)
        
        let val5 = calculate 1 (inputValues |> reduceWithYOffset 2)
        val1 * val2 * val3 * val4 * val5
