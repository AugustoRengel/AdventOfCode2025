namespace Challenges

type InitializationProcedure = private {
    LightDiagram: char array //[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    ButtonWiringSchematics: int array list
    JoltageRequirements: int array
}

module InitializationProcedure =
    open Utils.Combinatorics
    open System.Collections.Generic

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

    let pressedButtonsMatchJoltageRequirements (pressedButtons: int array list) (target: int array) =
        let stateOfJoltage = Array.init target.Length (fun _ -> 0)

        pressedButtons
        |> List.iter (fun switchedLights ->

            switchedLights
            |> Array.iter (fun i -> stateOfJoltage.[i] <- stateOfJoltage.[i] + 1))

        stateOfJoltage = target

    let chooseButton (buttons: list<array<int>>) (joltageDiff: int array) (bannedIds: int array) =
        let biggestDiff = Array.max joltageDiff

        let desiredIndexes =
            joltageDiff
            |> Array.mapi (fun i v -> i, v = biggestDiff)
            |> Array.filter snd
            |> Array.map fst

        let result =
            buttons
            |> List.mapi (fun i arr -> i, arr)
            |> List.filter (fun (i, _) -> not (Array.contains i bannedIds))
            |> List.map (fun (i, arr) ->
                let numberOfDesiredIndexes =
                    desiredIndexes |> Array.filter (fun v -> Array.contains v arr) |> Array.length

                (i, arr), numberOfDesiredIndexes)
            |> List.sortByDescending (fun (_, numberOfDesiredIndexes) -> numberOfDesiredIndexes)
            |> List.map fst
            |> List.tryFind (fun (_, arr) ->
                let dontOverflow = arr |> Array.forall (fun v -> joltageDiff.[v] > 0)
                dontOverflow)

        result

    let whichButtonsAffect (index: int) (buttonsValues: list<array<int>>) =
        buttonsValues
        |> List.mapi (fun i arr -> if Array.contains index arr then Some i else None)
        |> List.choose id
        |> List.toArray

    // {3,5,4,7}
    // {3,5,4,6}
    // (3) (1,3) (2) (2,3) (0,2) (0,1)
    // v
    // * [a,b,c,d,e,f]
    //   [0,2,0,0,0,3]

    // i=0 -> e + f = 3 = 0,0,0,0,1,1
    // i=1 -> b + f = 5
    // i=2 -> c + d + e = 4
    // i=3 -> a + b + d = 7

    // a + 2b + c + 2d + 2e + 2f = 19

    // b = 5 - f
    // b = 7 - a - c
    // 2b = 12 - a - c - f
    let getEquations (target: int array) (buttonPresses: int array) (buttonsValues: list<array<int>>) = ()

    let joltageEquations (target: int array) (buttonPresses: int array) (buttonsValues: list<array<int>>) =
        target
        |> Array.mapi (fun i targetJoltage ->
            targetJoltage, whichButtonsAffect i buttonsValues |> Array.map (fun j -> buttonPresses.[j]))

    let tryPressingNButtonsToMatchJoltageRequirements (procedure: InitializationProcedure) (verbose: bool) =
        let initialMatrix =
            let lines = procedure.JoltageRequirements.Length
            let colns = procedure.ButtonWiringSchematics.Length
            Array2D.init lines colns (fun i j -> 0)

        let target = procedure.JoltageRequirements

        procedure.ButtonWiringSchematics
        |> List.iteri (fun j arr -> arr |> Array.iter (fun i -> initialMatrix.[i, j] <- 1))

        printfn "%A" initialMatrix
        printfn "%A" target

        Utils.Matrix.gaussianElimination initialMatrix target

    let tryPressingNButtonsToMatchJoltageRequirements2 (procedure: InitializationProcedure) (verbose: bool) =
        let maxButtonPresses = Array.sum procedure.JoltageRequirements

        let expectedTotalJoltage = maxButtonPresses
        printfn "Total Joltage Required: %i" expectedTotalJoltage

        let sortedButtons =
            procedure.ButtonWiringSchematics
            |> List.sortByDescending (fun arr -> arr.Length)

        let mutable timesPressed = 0
        let mutable bannedIds = [||]
        let mutable hasButtonsToPress = true
        let mutable foundSolution = false
        let mutable tries = 1
        let joltageLevels = Array.init procedure.JoltageRequirements.Length (fun _ -> 0)
        let buttonsPressed = Stack<int * (int array)>()

        while timesPressed < maxButtonPresses && hasButtonsToPress && not foundSolution do
            let joltageDiff =
                procedure.JoltageRequirements |> Array.mapi (fun i v -> v - joltageLevels.[i])

            if verbose then
                printf "try %i | pressed %i | diff %A with bannedIds %A -> " tries timesPressed joltageDiff bannedIds

            tries <- tries + 1
            let choosedButton = chooseButton sortedButtons joltageDiff bannedIds

            match choosedButton with
            | Some(i, _) ->
                let lastBannedIds = [| i |]

                if verbose then
                    printfn "add button %i -> %A" i sortedButtons.[i]

                timesPressed <- timesPressed + 1
                buttonsPressed.Push(i, lastBannedIds)

                sortedButtons.[i]
                |> Array.iter (fun j -> joltageLevels.[j] <- joltageLevels.[j] + 1)

                bannedIds <- [||]
            | None ->
                let lastButtonIndex, lastBannedIds = buttonsPressed.Pop()

                printfn "remove button %i" lastButtonIndex

                timesPressed <- timesPressed - 1
                bannedIds <- lastBannedIds

                sortedButtons.[lastButtonIndex]
                |> Array.iter (fun j -> joltageLevels.[j] <- joltageLevels.[j] - 1)

            if joltageLevels = procedure.JoltageRequirements then
                printfn "Found Solution!"
                foundSolution <- true

            if buttonsPressed.Count = 0 then
                printfn "Dont have more buttons to try"
                hasButtonsToPress <- false

            // printfn "stack size %i" buttonsPressed.Count
            System.Console.ReadLine() |> ignore

        if foundSolution then Some timesPressed else None


module day10 =
    open System.Diagnostics

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

    let matchLightDiagram (filePath: string) =
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

    let matchJoltageRequirements (filePath: string) (verbose: bool) =
        let rows = System.IO.File.ReadAllLines filePath
        let data = rows |> Array.map parseRow

        data
        |> Array.map (fun procedure ->
            let sw = Stopwatch.StartNew()

            let result =
                InitializationProcedure.tryPressingNButtonsToMatchJoltageRequirements procedure verbose

            sw.Stop()

            printfn "%A" sw.Elapsed

        // match result with
        // | Some r ->
        //     printfn "%i presses\n" r
        //     r
        // | None ->
        //     printfn "Dont have solutions\n"
        //     0
        )
    // |> Array.sum
    // |> printfn "Total: %i"

    let run (filePath: string, verbose: bool) =
        // matchLightDiagram filePath
        matchJoltageRequirements filePath verbose

        ""
// (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
// [0,0,0,1] [0,1,0,1] [0,0,1,0] [0,0,1,1] [1,0,1,0] [1,1,0,0]
// * [a,b,c,d,e,f]
//   [1,3,0,3,1,2]

// b = 5 - f
// b = 7 - a - c
// 2b = 12 - a - c - f

// i=0 -> e + f = 3
// i=1 -> b + f = 5
// i=2 -> c + d + e = 4
// i=3 -> a + b + d = 7

// a = 7

// a + b + c + d + e + f = ?
