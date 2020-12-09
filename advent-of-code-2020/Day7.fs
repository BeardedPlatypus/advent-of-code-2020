namespace advent_of_code_2020

open System

module Day7 =
    type Bag =
        { Color: String
          Contains: (String * Int32) array
        }
        
    let toBag (inp: String): Bag =
        let inp = inp.TrimEnd('.')
        
        let sInp = inp.Split("contain", 2, StringSplitOptions.RemoveEmptyEntries)
        let color = sInp.[0].Substring(0, sInp.[0].Length - 5).Trim()
        let containData = sInp.[1]
        
        let containedColors  =
            if containData.Trim() = "no other bags" then
                Array.empty
            else 
                let containedColorsSplit = containData.Split(",", StringSplitOptions.RemoveEmptyEntries)
                
                let toContainsValue (s: string): (String * Int32) =
                    let v = s.Trim().Split(" ", 2)
                    let count = Int32.Parse v.[0]
                    let c = if count = 1 then v.[1].Substring(0, v.[1].Length - 4) else v.[1].Substring(0, v.[1].Length - 5)
                    
                    c, count
                    
                Array.map toContainsValue containedColorsSplit
        
        { Color = color
          Contains = containedColors
        }
    
    let inputValues : Bag list =
        System.IO.File.ReadAllText("resources/day7/input.txt").Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map toBag
        |> Array.toList
    
    
    // TODO: rewrite this with a map?
    let rec computeRec (hasGold: String Set) (cantHaveGold: String Set) (bags: Bag list): String Set =
        match bags with
        | [] -> hasGold
        | h :: tail ->
            let newContainedColours = Array.filter (fun (s, _) -> not <| cantHaveGold.Contains s) h.Contains
            
            let newHasGold, newCantHaveGold, newBags =
                if Array.isEmpty newContainedColours then
                    hasGold, cantHaveGold.Add h.Color, tail
                elif Array.exists (fun (s, _) -> s = "shiny gold" || hasGold.Contains s) newContainedColours then
                    hasGold.Add h.Color, cantHaveGold, tail
                else
                    hasGold, cantHaveGold, [ yield! tail; { h with Contains = newContainedColours } ]
                    
            computeRec newHasGold newCantHaveGold newBags
    
    let compute = computeRec Set.empty Set.empty
    
    let public calculateProblem1 () : int =
        compute inputValues
        |> Set.count
                    
    let rec computeRec2 (bagCountMap: Map<string, int32>) (bags: (Bag * int32) list) : int =
        match bags with
        | [] -> bagCountMap.Item "shiny gold" - 1
        | (b, i) :: tail ->
            let newI, newContainedColours =
                let withKey, withoutKey = Array.partition (fun (s, _) -> bagCountMap.ContainsKey s) b.Contains
                
                let fFold acc (s, v) = acc + (bagCountMap.Item s) * v
                Array.fold fFold i withKey, withoutKey
                
            let newMap, newBags =
                if Array.isEmpty newContainedColours then
                    bagCountMap.Add (b.Color, newI + 1), tail
                else
                    bagCountMap, [yield! tail; yield ({ b with Contains = newContainedColours }, newI)]
            
            computeRec2 newMap newBags
            
    let compute2 = computeRec2 Map.empty
    
    let public calculateProblem2 () : int =
        compute2 (List.map (fun b -> (b, 0)) inputValues)

