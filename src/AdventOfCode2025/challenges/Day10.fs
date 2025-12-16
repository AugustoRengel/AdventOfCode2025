namespace Challenges

type InitializationProcedure = private {
    LightDiagram: char array list //[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    ButtonWiringSchematics: int array list
    JoltageRequirements: int array list
}

module InitializationProcedure =
    let create lightDiagram buttonWiringSchematics joltageRequirements = {
        LightDiagram = lightDiagram
        ButtonWiringSchematics = buttonWiringSchematics
        JoltageRequirements = joltageRequirements
    }

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

                    {
                        acc with
                            LightDiagram = content :: acc.LightDiagram
                    }
                | '(' ->
                    let content = s.[1 .. n - 2].Split "," |> Array.map int

                    {
                        acc with
                            ButtonWiringSchematics = content :: acc.ButtonWiringSchematics
                    }
                | '{' ->
                    let content = s.[1 .. n - 2].Split "," |> Array.map int

                    {
                        acc with
                            JoltageRequirements = content :: acc.JoltageRequirements
                    }
                | _ -> failwith "Array doesn't start with [, ( or {")
            (InitializationProcedure.create [] [] [])

    let run (filePath: string, verbose: bool) =
        let rows = System.IO.File.ReadAllLines filePath
        let data = rows |> Array.map parseRow
        printfn "%A" data
        ""
