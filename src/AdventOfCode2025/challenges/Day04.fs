namespace Challenges

module day04 = 
    let rangeAround (value: int) (range: int) =
        List.init (2 * range + 1) (fun k -> value - range + k)
    let getAdjacentPositions(position: int * int, proximity: int, matrixDimensions: int * int) =
        let i, j = position
        let iMax, jMax = matrixDimensions

        let iList = rangeAround i proximity
        let jList = rangeAround j proximity
        List.allPairs iList jList
        |> List.filter (fun (r, c) -> 
            r >= 0 && 
            r < iMax &&
            c >= 0 && 
            c < jMax
            )
        |> List.except [position]

    let isRollAccessible (grid: char array2d, position: int * int, gridDimension: int * int) =
        let adjacentValues = 
            getAdjacentPositions (position, 1, gridDimension) 
            |> List.fold (fun acc (r, c) -> 
                if grid[r,c] = '@' || grid[r,c] = 'x' then 
                    acc + 1
                else
                    acc
                ) 0
        if adjacentValues < 4 then
            true
        else
            false

    let run(filePath: string, verbose: bool, repeat: bool) =
        let dataRows = System.IO.File.ReadAllLines filePath
        
        let matrixDimensions = 
            dataRows.Length, dataRows.[0].Length

        let mutable matrix =
            let rows, cols = matrixDimensions
            Array2D.init rows cols (fun r c -> dataRows.[r].[c])
        
        
        let mutable removedRolls = 1
        let mutable totalRemovedRolls = 0

        while removedRolls > 0 do
            removedRolls <- 0
            let newMatrix =
                matrix
                |> Array2D.mapi (fun i j value -> 
                    match value with
                    |'@' -> 
                        let isAccessible = 
                            isRollAccessible (
                                matrix, 
                                (i, j), 
                                matrixDimensions
                            )
                        if isAccessible then
                            'x'
                        else
                            value
                    | '.' | _ -> value
                    )
                |> Array2D.map (fun x -> 
                    if x = 'x' then
                        removedRolls <- removedRolls + 1
                        '.'
                    else
                        x
                    )
            matrix <- newMatrix
            if verbose then
                printfn "%i Rolls accessible" removedRolls
            totalRemovedRolls <- totalRemovedRolls + removedRolls
            if not repeat then
                removedRolls <- 0
            
        sprintf "%i" totalRemovedRolls