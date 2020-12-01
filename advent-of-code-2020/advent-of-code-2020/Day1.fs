namespace advent_of_code_2020

module Day1 =
    let inputValues () : int list =
        System.IO.File.ReadLines("resources/day1/input.txt")
        |> List.ofSeq
        |> List.map int
        |> List.sort
    
    let rec findPairSpecificRec (goalValue: int) (lhs: int) (rhsList: int list) : (int * int) Option  =
        match rhsList with
        | [] -> None
        | rhs :: _ when (rhs + lhs) = goalValue -> Some (rhs, lhs)
        | rhs :: _ when (rhs + lhs) > goalValue -> None
        | _   :: tail -> findPairSpecificRec goalValue lhs tail
        
    let rec findPairRec (goalValue: int) (lhsList: int list) (rhsList: int list) : (int * int) Option =
        if lhsList |> List.isEmpty then
            None
        else
            let res = findPairSpecificRec goalValue (List.head lhsList) rhsList
            
            match res with
            | Some _ -> res
            | None   -> findPairRec goalValue (List.tail lhsList) rhsList
        
    let findPair (goalValue: int) (input: int list) : (int * int) Option =
        let reversed = input |> List.rev
        
        findPairRec goalValue reversed input
        
    let public calculateProblem1 () =
        let inputValues = inputValues ()
        let res = findPair 2020 inputValues

        match res with
        | Some (lhs, rhs) -> lhs * rhs 
        | None            -> -1
        
    let yieldProblem2 (goalValue: int) (input: int list) (nValues: int) : (int * int) Option =
        let mutable elements = List.zip input input
        for i in 2 .. nValues do
            elements <- [ for (e1, e2) in elements do
                          for i in input do
                          if (e1 + i) <= goalValue then yield ((e1 + i), (e2 * i))
                        ]
                
        List.tryFind (fun (e1, _) -> e1 = goalValue) elements
                
    let public calculateProblem2 () =
        let inputValues = inputValues ()
        let res = yieldProblem2 2020 inputValues 2

        match res with
        | Some (_, res) -> res
        | None          -> -1
        
    let public calculateProblem3 () =
        let inputValues = inputValues ()
        let res = yieldProblem2 2020 inputValues 3

        match res with
        | Some (_, res) -> res
        | None          -> -1
        

