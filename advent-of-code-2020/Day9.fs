namespace advent_of_code_2020

open System

module Day9 =
    let inputValues : int64 list =
        System.IO.File.ReadAllText("resources/day9/input.txt").Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int64.Parse
        |> Array.toList
        
    let rec computeElem (goal: int64) (currElems: int64 list) : bool =
        [ for e1 in currElems do
          for e2 in currElems do
          if e1 <> e2 then yield e1 + e2
        ]
        |> Set.ofList
        |> Set.contains goal
        
    let rec compute1 (currElems: int64 list) (nextElems: int64 list): int64 option =
        match nextElems with
        | [] -> None
        | nextHead :: nextTail -> 
            if computeElem nextHead currElems then
                compute1 (List.tail currElems @ [ nextHead ]) nextTail
            else
                Some nextHead
                
    let public calculateProblem1 () : int64 =
        compute1 (List.take 25 inputValues) (List.skip 25 inputValues)
        |> Option.defaultValue (int64 -1)
        
    let rec compute2 (goalVal: int64) (accVal: int64) (currElems: int64 list) (nextElems: int64 list): (int64 list) option =
        if accVal = goalVal then
            if List.length currElems > 1 then
                Some currElems
            else
                compute2 goalVal (int64 0) [] nextElems
        elif accVal > goalVal then
            match currElems with
            | [] -> None
            | h :: t -> compute2 goalVal (accVal - h) t nextElems
        else
            match nextElems with
            | [] -> None
            | h :: t -> compute2 goalVal (accVal + h) [ yield! currElems; yield h ] t
    
    let public calculateProblem2 () : int64 =
        let goalValue = calculateProblem1 ()
        
        compute2 goalValue (int64 0) [] inputValues
        |> Option.map List.sort
        |> Option.map (fun l -> List.head l + List.last l)
        |> Option.defaultValue (int64 -1)
        