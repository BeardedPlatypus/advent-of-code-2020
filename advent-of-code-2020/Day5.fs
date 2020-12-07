namespace advent_of_code_2020

open System

module Day5 =
    type Seat =
        { Row : int
          Col : int
        }
        
    let calcValue (c: char) (inp: char seq): int =
        let toInt (c: char) (v: char) = if v = c then 0 else 1
        let toVal (i: int) (v: int) = v <<< i
        
        inp
        |> Seq.map (toInt c)
        |> Seq.rev
        |> Seq.mapi toVal
        |> Seq.sum
        
    let toSeat (inp: string) : Seat =
        let row: int = Seq.take 7 inp |> calcValue 'F'
        let col: int = Seq.skip 7 inp |> calcValue 'L'
        
        { Row = row
          Col = col
        }

    let inputValues () =
        System.IO.File.ReadAllText("resources/day5/input.txt").Split("\n")
        |> Array.map toSeat
    let public calculateProblem1 () =
        let inputValues = inputValues ()
        
        let foldFun (curHigh: int) (seat: Seat): int =
            let v = seat.Row * 8 + seat.Col
            if v > curHigh then v else curHigh
        
        Array.fold foldFun -1 inputValues

    let public calculateProblem2 () =
        let inputValues =
            inputValues ()
            |> Array.map (fun s -> s.Row * 8 + s.Col)
            |> Array.sort
        
        let findPred (i: int, v: int) = i <> v
        
        let offset = Array.head inputValues
        let _, res = Array.zip [| offset .. (Array.length inputValues) + offset - 1 |] inputValues
                     |> Array.find findPred
        
        res - 1