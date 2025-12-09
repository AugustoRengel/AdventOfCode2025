namespace Challenges

type ReadMethod =
    | Human
    | Cephalopod

module day06 =
    open System
    open System.Text.RegularExpressions

    let reverseString(s: string) =
        s.ToCharArray()
        |> Array.rev
        |> fun arr -> String(arr)

    let transpose(matrix: 'T array array) =
        let rows = matrix.Length
        let cols = matrix.[0].Length
        Array.init cols (fun j ->
            Array.init rows (fun i ->
                matrix.[i].[j]
            )
        )

    let solveCephalopodProblem(arr: string array) =
        let numbers = arr.[0..arr.Length - 2] |> Array.map int64
        let operator = arr.[arr.Length - 1]
        let operation, initialAcc =
            match operator with
            | "*" -> (fun acc v -> acc * v), 1L
            | "+" -> (fun acc v -> acc + v), 0L
            | "-" -> (fun acc v -> acc - v), 0L
            | _ -> failwith "Unknow operation"

        numbers
        |> Array.fold operation initialAcc

    let run(filePath: string, verbose: bool, method: ReadMethod) =
        let rows = System.IO.File.ReadAllLines filePath

        let parsedProblems =
            match method with
            | Cephalopod -> 
                rows
                |> Array.map reverseString
                |> Array.map (fun s -> s.ToCharArray())
                |> transpose
                |> Array.map (fun arr -> String(arr).Trim())
                |> Array.fold (fun (acc, arr) s ->
                    if s = "" then
                        Array.append acc [|arr|], [||]
                    else
                        acc, Array.append arr [|s|]
                    ) ([||], [||])
                |> fun (acc, arr) -> 
                    if arr.Length <> 0 then
                        Array.append acc [|arr|]
                    else
                        acc
                |> Array.map (fun arr ->
                    let lastItem = arr.[arr.Length - 1]
                    let operator = string lastItem.[lastItem.Length - 1]
                    arr.[arr.Length - 1] <- lastItem.Replace(operator, "")
                    Array.append arr [|operator|])
            | Human |_ ->
                rows
                |> Array.map (fun s -> Regex.Replace(s, "\s+", " "))
                |> Array.map (fun s -> s.Trim().Split " ")
                |> transpose

        if verbose then
            parsedProblems
            |> Array.iter (printfn "%A")

        parsedProblems
        |> Array.map solveCephalopodProblem
        |> Array.sum
        |> sprintf "%i"