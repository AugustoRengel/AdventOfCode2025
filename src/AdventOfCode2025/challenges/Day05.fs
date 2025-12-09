namespace Challenges

module day05 =
    let rangeContains(range: (int64*int64) array, value: int64) =
        range
        |> Array.exists (fun (first, last) ->
            value >= first && 
            value <= last
            )
    let tryCombineRanges(r1: int64*int64, r2: int64*int64): option<int64*int64> =
        let startR1, endR1 = r1
        let startR2, endR2 = r2

        let isStartR2isInR1 = 
            startR2 >= startR1 && 
            startR2 <= endR1

        let isEndR2inR1 = 
            endR2 >= startR1 && 
            endR2 <= endR1

        let isR1inR2 =
            startR2 < startR1 && 
            endR2 > endR1
            
        if not isStartR2isInR1 && not isEndR2inR1 && not isR1inR2 then
            None
        else
            let newStart =
                if isStartR2isInR1 then
                    startR1
                else
                    startR2
            let newEnd =
                if isEndR2inR1 then
                    endR1
                else
                    endR2
            Some (newStart, newEnd)

    let run(filePath: string, verbose: bool, onlyFreshIds: bool) =
        let rows = System.IO.File.ReadAllLines(filePath)

        let ranges, ids = 
            match Array.tryFindIndex (fun s -> s = "") rows with
            | Some index ->
                rows[.. index-1], 
                rows[index+1 ..] |> Array.map int64
            | None ->
                rows, [||]

        let freshIds =
            ranges
            |> Array.map( fun idRange -> idRange.Split "-")
            |> Array.map( fun idRange -> 
                let first = int64 idRange.[0]
                let second = int64 idRange.[1]
                first, second)
            |> Array.sortBy fst
            |> Array.fold( fun acc v ->
                if Array.isEmpty acc then
                    [|v|]
                else
                    let lastIndex = acc.Length - 1
                    match tryCombineRanges(v, acc.[lastIndex]) with
                    | None -> Array.append acc [|v|]
                    | Some newRange ->
                        acc.[lastIndex] <- newRange
                        acc
                ) [||]
        let totalFreshIds =
            freshIds
            |> Array.fold (fun acc (startRange, endRange) ->
                acc + endRange - startRange + 1L) 0L

        let totalFreshItems =
            ids
            |> Array.fold (fun acc v ->
                if rangeContains(freshIds, v) then
                    acc + 1
                else
                    acc
                ) 0
        if onlyFreshIds then
            sprintf "%i" totalFreshIds
        else
            sprintf "%i" totalFreshItems