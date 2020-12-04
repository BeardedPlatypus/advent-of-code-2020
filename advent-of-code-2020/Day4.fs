namespace advent_of_code_2020

open System

module Day4 =
    type passportData =
        { BYR: string Option
          IYR: string Option
          EYR: string Option
          HGT: string Option
          HCL: string Option
          ECL: string Option
          PID: string Option
          CID: string Option
        }
        
    let emptyPassportData =
        { BYR = None
          IYR = None
          EYR = None
          HGT = None
          HCL = None
          ECL = None
          PID = None
          CID = None
        }
        
    let updateElement (data: passportData) (inpVal: string) : passportData =
        match inpVal.Split(':', 2) with
        | [| k; v |] when k = "byr" -> { data with BYR = Some v }
        | [| k; v |] when k = "iyr" -> { data with IYR = Some v }
        | [| k; v |] when k = "eyr" -> { data with EYR = Some v }
        | [| k; v |] when k = "hgt" -> { data with HGT = Some v }
        | [| k; v |] when k = "hcl" -> { data with HCL = Some v }
        | [| k; v |] when k = "ecl" -> { data with ECL = Some v }
        | [| k; v |] when k = "pid" -> { data with PID = Some v }
        | [| k; v |] when k = "cid" -> { data with CID = Some v }
        | _ -> data
        
    let toElem (inp: string): passportData =
        let inputValues = inp.Split([|" "; "\n"|], StringSplitOptions.RemoveEmptyEntries)
        Array.fold updateElement emptyPassportData inputValues
    
    let inputValues () =
        System.IO.File.ReadAllText("resources/day4/input.txt").Split("\n\n")
        |> Array.map toElem
    
    let isValidPart1 (data : passportData) : bool =
        data.BYR |> Option.isSome &&
        data.IYR |> Option.isSome &&
        data.EYR |> Option.isSome &&
        data.HGT |> Option.isSome &&
        data.HCL |> Option.isSome &&
        data.ECL |> Option.isSome &&
        data.PID |> Option.isSome 
    
    let public calculateProblem1 () =
        let inputValues = inputValues ()
        
        Seq.filter isValidPart1 inputValues
        |> Seq.length
        
    let isValidInt (lowerBound: int) (upperBound: int) (data: String Option) : bool =
        match data with
        | Some d ->
            let b, n = Int32.TryParse d
            b && n >= lowerBound && n <= upperBound
        | None ->
            false
        
    let isValidBYR = isValidInt 1920 2002
    let isValidIYR = isValidInt 2010 2020 
    let isValidEYR = isValidInt 2020 2030 
       
    let isValidHeight (data: String option) =
        match data with
        | Some d when d.EndsWith("cm") ->
            let b, n = Int32.TryParse (d.Remove(d.Length - 2))
            b && n >= 150 && n <= 193
        | Some d when d.EndsWith("in") ->
            let b, n = Int32.TryParse (d.Remove(d.Length - 2))
            b && n >= 59 && n <= 76
        | _ ->
            false
            
    let isValidHCL (data: String option) =
        match data with
        | Some d when d.StartsWith("#") ->
            let s = d.Substring(1)
            s.Length = 6 && (Seq.forall (fun (c: char) -> List.contains c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f']) s)
        | _ ->
            false
            
    let isValidECL (data: String option) =
        match data with
        | Some d ->
            List.contains d ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        | _ ->
            false
            
    let isValidPID (data: String option) =
        match data with
        | Some d ->
            d.Length = 9 && (Seq.forall (fun (c: char) -> List.contains c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']) d)
        | _ ->
            false
        
    let isValidPart2 (data : passportData) : bool =
        data.BYR |> isValidBYR &&
        data.IYR |> isValidIYR &&
        data.EYR |> isValidEYR &&
        data.HGT |> isValidHeight &&
        data.HCL |> isValidHCL &&
        data.ECL |> isValidECL &&
        data.PID |> isValidPID
        
    let public calculateProblem2 () =
        let inputValues = inputValues ()
        
        Seq.filter isValidPart2 inputValues
        |> Seq.length
