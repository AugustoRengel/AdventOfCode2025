module TestsHelpers

open System.IO

let testDir: string = __SOURCE_DIRECTORY__

let solutionRoot: string = 
    testDir
    |> Directory.GetParent
    |> fun (d: DirectoryInfo) -> d.Parent
    |> fun (d: DirectoryInfo) -> d.FullName

let pathFromTestDir (parts: string list) =
    Path.Combine(testDir, Path.Combine(parts |> Array.ofList))