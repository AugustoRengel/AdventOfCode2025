namespace Utils

module Combinatorics =
    let rec combinations k list =
        match k, list with
        | k, _ when k > List.length list -> []
        | 0, _ -> [ [] ]
        | _, [] -> []
        | k, head :: body ->
            let withHead = combinations (k - 1) body |> List.map (fun c -> head :: c)

            let withoutHead = combinations k body

            withHead @ withoutHead

module PathHelper =
    open System.IO

    let utilsDir: string = __SOURCE_DIRECTORY__

    let solutionRoot: string =
        utilsDir
        |> Directory.GetParent
        |> fun (d: DirectoryInfo) -> d.Parent
        |> fun (d: DirectoryInfo) -> d.Parent
        |> fun (d: DirectoryInfo) -> d.FullName

    let pathFromRoot (parts: string list) =
        Path.Combine(solutionRoot, Path.Combine(parts |> Array.ofList))

    let getDataFile (fileName: string) =
        let parts = [ "src"; "AdventOfCode2025"; "data"; fileName ]
        Path.Combine(solutionRoot, Path.Combine(parts |> Array.ofList))
