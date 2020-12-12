namespace advent_of_code_2020

open System

module Day10 =
    let inputValues : int32 list =
        System.IO.File.ReadAllText("resources/day10/input.txt").Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
        |> Array.sort
        |> Array.toList
        
    let public calculateProblem1 () : int32 =
        let inp = inputValues
        let fFold (j1: int32, j2: int32, j3: int32) (v1: int32) (v2: int32): int32 * int32 * int32 =
            match v2 - v1 with
            | 1 -> (j1 + 1, j2, j3)
            | 2 -> (j1, j2 + 1, j3)
            | 3 -> (j1, j2, j3 + 1)
            | _ -> (j1, j2, j3)
        let j1, j2, j3 = List.fold2 fFold (0, 0, 0) (0 :: inp)  ( inp @ [ List.last inp + 3 ])
        j1 * j3
        
    let foldCompute ((v1: int32, c1: int64), (v2: int32, c2: int64), (v3: int32, c3: int64)) (vNew: int32) : ((int32 * int64) * (int32 * int64) * (int32 * int64)) =
        let newElem = (vNew,
                       (if vNew - v1 <= 3 then c1 else int64 0) +
                       (if vNew - v2 <= 3 then c2 else int64 0) +
                       (if vNew - v3 <= 3 then c3 else int64 0))
        ((v2, c2), (v3, c3), newElem)
        
    let public calculateProblem2 () : int64 =
        let (_, _, (_, res)) =
            List.fold foldCompute ((0, int64 0), (0, int64 0), (0, int64 1)) inputValues
        res 
        
        
        
