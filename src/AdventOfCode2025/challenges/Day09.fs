namespace Challenges


module day09 =
    let rectangleArea p1 p2 =
        let x1, y1 = p1
        let x2, y2 = p2
        (1L + int64 (abs(x1 - x2))) * (1L + int64 (abs(y1 - y2)))

    let generateWallRules (p1) (p2): ((int*int*int*int->bool)) =
        let x1, y1 = p1
        let x2, y2 = p2

        match x1 = x2, y1 = y2 with
        | true, false -> // vertical alignment between points
            fun (minX, maxX, minY, maxY) -> // wall intersect rectangle
                let startY = min y1 y2
                let endY = max y1 y2
                x1 > minX &&
                x1 < maxX &&
                ((startY > minY && startY < maxY) ||
                (endY > minY && endY < maxY) ||
                (startY <= minY && endY >= maxY))
        | false, true -> // horizontal alignment between points
            fun (minX, maxX, minY, maxY) -> // wall intersect rectangle
                let startX = min x1 x2
                let endX = max x1 x2
                y1 > minY &&
                y1 < maxY &&
                ((startX > minX && startX < maxX) ||
                (endX > minX && endX < maxX) ||
                (startX <= minX && endX >= maxX))
        | _ -> fun _ -> false

    let parseRow(s: string) =
        let splitedRow = s.Trim().Split ","
        int splitedRow.[0], int splitedRow.[1]

    let run(filePath: string, verbose: bool) =
        let rows = System.IO.File.ReadAllLines filePath
        let redTilesLocation = 
            rows
            |> Array.map parseRow
        
        let n = redTilesLocation.Length

        let walls =
            redTilesLocation
            |> Array.mapi (fun i position ->
                let j = if i + 1 >= n then 0 else i+1
                generateWallRules position redTilesLocation.[j])

        let mutable counter = 0L
        let totalAcc = n * (n - 1) / 2

        let redTilesAreas =
            redTilesLocation
            |> Array.mapi (fun i position ->
                seq {i+1..n-1}
                |> Seq.map (fun j ->
                    printfn "%i / %i - Area" counter totalAcc
                    counter <- counter + 1L

                    let area = rectangleArea position redTilesLocation.[j]
                    area, position, redTilesLocation.[j]
                    )
                )
                |> Seq.collect id
            |> Seq.sortByDescending (fun (area, _, _) -> area)

        let biggestValidArea =
            redTilesAreas
            |> Seq.tryFind (fun (area, p1, p2) ->
                let minX = min (fst p1) (fst p2)
                let maxX = max (fst p1) (fst p2)
                let minY = min (snd p1) (snd p2)
                let maxY = max (snd p1) (snd p2)
                let rectangleHasInternalRedTile =
                    walls
                    |> Array.exists (fun rectangleHasWallInside -> 
                        rectangleHasWallInside (minX, maxX, minY, maxY)
                        )
                not rectangleHasInternalRedTile
                )

        match biggestValidArea with
        | Some (area, p1, p2) -> printfn "biggest area is between %A * %A = %i" p1 p2 area
        | None -> printfn "None valid area"
        
        ""