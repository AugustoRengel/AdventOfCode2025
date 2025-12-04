namespace Utils

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

    let getDataFile(fileName: string) =
        let parts = ["src"; "AdventOfCode2025"; "data"; fileName]
        Path.Combine(solutionRoot, Path.Combine(parts |> Array.ofList))
