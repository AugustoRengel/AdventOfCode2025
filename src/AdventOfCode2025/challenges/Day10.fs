namespace Challenges

type InitializationProcedure = private {
    LightDiagram: char array //[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    ButtonWiringSchematics: int array list
    JoltageRequirements: int array
}

module InitializationProcedure =
    open Utils.Combinatorics

    let create lightDiagram buttonWiringSchematics joltageRequirements = {
        LightDiagram = lightDiagram
        ButtonWiringSchematics = buttonWiringSchematics
        JoltageRequirements = joltageRequirements
    }

    let pressedButtonsMatchLightDiagram (pressedButtons: int array list) (procedure: InitializationProcedure) =
        let target =
            procedure.LightDiagram |> Array.map (fun c -> if c = '#' then true else false)

        let stateOfLights = Array.init procedure.LightDiagram.Length (fun _ -> false)

        pressedButtons
        |> List.iter (fun switchedLights ->
            switchedLights
            |> Array.iter (fun i -> stateOfLights.[i] <- not stateOfLights.[i]))

        stateOfLights = target


    let tryPressingNButtonsToMatchLightDiagram (n) (procedure: InitializationProcedure) =
        let buttonsCombinations = combinations n procedure.ButtonWiringSchematics

        buttonsCombinations
        |> List.tryFind (fun pressedButtons -> pressedButtonsMatchLightDiagram pressedButtons procedure)


module day10 =
    let parseRow (row: string) =
        let splitedRow = row.Split " "

        splitedRow
        |> Array.fold
            (fun acc (s: string) ->
                let n = s.Length

                if n < 3 then
                    failwith "Expected at least 3 values in row"

                match s.[0] with
                | '[' ->
                    let content = s.[1 .. n - 2].ToCharArray()

                    { acc with LightDiagram = content }
                | '(' ->
                    let content = s.[1 .. n - 2].Split "," |> Array.map int

                    {
                        acc with
                            ButtonWiringSchematics = acc.ButtonWiringSchematics @ [ content ]
                    }
                | '{' ->
                    let content = s.[1 .. n - 2].Split "," |> Array.map int

                    {
                        acc with
                            JoltageRequirements = content
                    }
                | _ -> failwith "Array doesn't start with [, ( or {")
            (InitializationProcedure.create [||] [] [||])

    let run (filePath: string, verbose: bool) =
        let rows = System.IO.File.ReadAllLines filePath
        let data = rows |> Array.map parseRow

        data
        |> Array.map (fun procedure ->
            let totalButtons = procedure.ButtonWiringSchematics.Length

            seq { 0 .. totalButtons - 1 }
            |> Seq.tryPick (fun n -> InitializationProcedure.tryPressingNButtonsToMatchLightDiagram n procedure)
            |> fun result ->
                printf "%A -> " procedure.LightDiagram

                match result with
                | Some r ->
                    printfn "%i" r.Length
                    r.Length
                | None ->
                    printfn "Dont have solutions"
                    0)
        |> Array.sum
        |> printfn "Total: %i"

        ""
