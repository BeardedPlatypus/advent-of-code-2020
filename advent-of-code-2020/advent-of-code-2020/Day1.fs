namespace advent_of_code_2020

module Day1 =
    let inputValues () : int list =
        System.IO.File.ReadLines("resources/day1/input.txt")
        |> List.ofSeq
        |> List.map int
        |> List.sort
        
    // Note that this approach potentially counts the same element in the list twice, which
    // might be problematic, luckily such a situation did not occur with the provided input.
    let calculate (goalValue: int) (input: int list) (nValues: int) : (int * int) Option =
        let mutable elements = List.zip input input
        for i in 2 .. nValues do
            elements <- [ for (e1, e2) in elements do
                          for i in input do
                          if (e1 + i) <= goalValue then yield ((e1 + i), (e2 * i))
                        ]
                
        List.tryFind (fun (e1, _) -> e1 = goalValue) elements
                
    let public calculateProblem1 () =
        let inputValues = inputValues ()
        let res = calculate 2020 inputValues 2

        match res with
        | Some (_, res) -> res
        | None          -> -1
        
    let public calculateProblem2 () =
        let inputValues = inputValues ()
        let res = calculate 2020 inputValues 3

        match res with
        | Some (_, res) -> res
        | None          -> -1
        

