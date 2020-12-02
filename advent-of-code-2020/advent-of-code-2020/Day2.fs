namespace advent_of_code_2020

module Day2 =
    type PasswordData =
        { Character: char
          LowerVal: int
          UpperVal: int
          PassString: string
        }
        
    let toPasswordData (input: string) : PasswordData Option =
        match input.Split([|' '|], 3) with
        | [| (rangeInput: string) ; (character: string) ; (passString: string) |] -> 
            match rangeInput.Split '-' with
            | [| lowerVal; upperVal|] ->
                Some { Character = character.[0]
                       LowerVal = int lowerVal
                       UpperVal= int upperVal
                       PassString = passString
                     }
            | _ ->
                None
        | _ -> None
        
    let validateProblem1 (data: PasswordData ) : bool =
        let count = Seq.filter ((=) data.Character) data.PassString
                    |> Seq.length
        count >= data.LowerVal && count <= data.UpperVal
        
    let validateProblem2 (data: PasswordData ) : bool =
        data.PassString.Length >= data.UpperVal &&
        ( data.PassString.[data.LowerVal - 1] = data.Character  &&
          data.PassString.[data.UpperVal - 1] <> data.Character ) ||
        ( data.PassString.[data.LowerVal - 1] <> data.Character  &&
          data.PassString.[data.UpperVal - 1] = data.Character )
        
    let inputValues ()  =
        System.IO.File.ReadLines("resources/day2/input.txt")
        |> Seq.map toPasswordData
        |> Seq.choose id
        
    let public calculateProblem1 () =
        let inputValues = inputValues ()
        
        Seq.filter validateProblem1 inputValues
        |> Seq.length
        
    let public calculateProblem2 () =
        let inputValues = inputValues ()
        
        Seq.filter validateProblem2 inputValues
        |> Seq.length
