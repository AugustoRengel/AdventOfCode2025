namespace Challenges

module day05 =
    let run(filePath: string, verbose: bool) =
        let rows = System.IO.File.ReadAllLines(filePath)
        let ranges, ids = 
            match Array.tryFindIndex (fun s -> s = "") rows with
            | Some index ->
                rows[.. index-1], rows[index+1 ..]
            | None ->
                rows, [||]
        printfn "%A" ranges
        printfn "---"
        printfn "%A" ids
        ""