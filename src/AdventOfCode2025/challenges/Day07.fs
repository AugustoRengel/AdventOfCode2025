namespace Challenges

module day07 =
    let moveTachyonBeam (currentRow: char array) (nextRow: char array) (timelines: int64 array)=
        let newTimelines = timelines.[*]
        let mutable beamSplits = 0
        currentRow
        |> Array.iteri (fun (i:int) (c: char) ->
            match c with
            |'|'|'S' -> 
                match nextRow.[i] with
                | '^' ->
                    newTimelines.[i] <- 0L

                    nextRow.[i-1] <- '|'
                    newTimelines.[i-1] <- newTimelines.[i-1] + timelines.[i]

                    nextRow.[i+1] <- '|'
                    newTimelines.[i+1] <- newTimelines.[i+1] + timelines.[i]
                    beamSplits <- beamSplits + 1
                | '.'| _ ->
                    nextRow.[i] <- '|'
            | _ -> ()
            )
        nextRow, beamSplits, newTimelines
    let run(filePath: string, verbose: bool) =
        let rows = System.IO.File.ReadAllLines filePath
        let matrix =
            Array2D.init rows.Length rows.[0].Length (fun i j ->
                rows.[i].[j])
        
        let mutable totalBeamSplits = 0
        let mutable timelines = 
            matrix.[0, *]
            |> Array.map (fun c ->
                match c with
                |'S' -> 1L
                | _ -> 0L
                )

        seq {0..(matrix.GetLength 0) - 2}
        |> Seq.iter (fun i ->
            let nextRow, beamSplitsCount, newTimelines = moveTachyonBeam matrix.[i, *] matrix.[i+1, *] timelines
            matrix.[i+1, *] <- nextRow
            totalBeamSplits <- totalBeamSplits + beamSplitsCount
            timelines <- newTimelines
            )

        printfn "beam splits: %i" totalBeamSplits

        timelines 
        |> Array.sum
        |> sprintf "timelines: %i"