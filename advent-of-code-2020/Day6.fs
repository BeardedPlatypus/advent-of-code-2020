namespace advent_of_code_2020

open System

module Day6 =
    let countUniqueAnswers (inp: string): int =
        let individualAnswers = inp.Split('\n')
        
        Array.fold (+) "" individualAnswers
        |> Seq.distinct
        |> Seq.length
        
    let public calculateProblem1 () : int =
       System.IO.File.ReadAllText("resources/day6/input.txt").Split("\n\n")
       |> Array.map countUniqueAnswers
       |> Array.sum
 
    let countUnionAnswers (inp: string): int =
        let individualAnswers = inp.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        
        let res = Array.map Set.ofSeq individualAnswers
                  |> Array.reduce Set.intersect 
                  |> Seq.length
        
        res
        
    let public calculateProblem2 () : int =
       System.IO.File.ReadAllText("resources/day6/input.txt").Split("\n\n")
       |> Array.map countUnionAnswers
       |> Array.sum