﻿
open System
open advent_of_code_2020

[<EntryPoint>]
let main argv =
    let result = Day1.calculateProblem1 ()
    printfn "%d" result
    
    let result = Day1.calculateProblem2 ()
    printfn "%d" result
    0 // return an integer exit code
