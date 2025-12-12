namespace Challenges

type JunctionBox =
    private {
        Id: int
        X: int64
        Y: int64
        Z: int64
        mutable Conn: int array
        MaxConn: int
    }

type Edge = 
    {
        IdA: int
        IdB: int
        Distance: int64
    }

module JunctionBox =
    
    let mutable JunctionBoxCount = 0
    
    let create(x, y, z) (maxConn) =
        let result =
            {
                Id=JunctionBoxCount;
                X=x;
                Y=y;
                Z=z;
                Conn=[||];
                MaxConn=maxConn;
            }
        JunctionBoxCount <- JunctionBoxCount + 1
        result
    
    let distanceBetween (jBoxA: JunctionBox, jBoxB: JunctionBox): Edge =
        let dx = jBoxA.X - jBoxB.X
        let dy = jBoxA.Y - jBoxB.Y
        let dz = jBoxA.Z - jBoxB.Z
        {
            IdA=jBoxA.Id
            IdB=jBoxB.Id
            Distance= dx*dx + dy*dy + dz*dz
        }


type Circuit = JunctionBox array

module Circuit =
    let create(jBoxId: JunctionBox): Circuit =
        [|jBoxId|]

module day08 =
    open System.Collections.Generic

    type Direction =
        |Right
        |Left
        |Both


    let propagate (juctionBoxes: Dictionary<int, JunctionBox>, id: int): int array =
        let mutable acc = [|id|]

        let rec recPropagate(juctionBoxes: Dictionary<int, JunctionBox>, id: int) =
            let jBox = juctionBoxes.[id]
            let connections = jBox.Conn

            connections
            |> Array.iter (fun jBoxId ->
                if not(Array.contains jBoxId acc) then
                    acc <- Array.append acc [|jBoxId|]
                    recPropagate(juctionBoxes, jBoxId) |> ignore
                )
        recPropagate(juctionBoxes, id)
        acc


    let parseRow (s: string) = 
        match s.Trim().Split "," with 
        | [|x; y; z|] ->
            int64 x, int64 y, int64 z
        | _ ->
            failwith "Expected 3 numerical values separeted by coma"
    
    let getShortestConnections (points: JunctionBox array) =
        let distances =
            points
            |> Array.mapi (fun i point ->
                seq {i+1..points.Length-1}
                |> Seq.toArray
                |> Array.map (fun j -> 
                    JunctionBox.distanceBetween(point, points.[j])))
            |> Array.concat
            |> Array.sortBy (fun e -> e.Distance)
        distances

    let run(filePath: string, verbose: bool, N: int) =
        let rows = System.IO.File.ReadAllLines filePath

        let mutable jBoxById = Dictionary<int, JunctionBox>()

        let juctionBoxes =
            rows
            |> Array.map parseRow
            |> Array.map (fun p ->
                let juctionBox = JunctionBox.create p -1
                jBoxById.[juctionBox.Id] <- juctionBox
                juctionBox
                )
                
        let shortestConnections = getShortestConnections juctionBoxes
        let mutable connectionCount = 0
        let mutable i = 0
        let mutable multipleCircuits = true
        let mutable result = ""
        while (connectionCount < N && i < shortestConnections.Length) || multipleCircuits do
            let conn: Edge = shortestConnections.[i]
            let jBoxA = jBoxById.[conn.IdA]
            let jBoxB = jBoxById.[conn.IdB]
            let jBoxAHasFreeConn = 
                if jBoxA.MaxConn = -1 then
                    true
                else
                    jBoxA.Conn.Length < jBoxA.MaxConn
            let jBoxBHasFreeConn = 
                if jBoxB.MaxConn = -1 then
                    true
                else
                    jBoxB.Conn.Length < jBoxB.MaxConn

            if jBoxAHasFreeConn && jBoxBHasFreeConn then
                if verbose then
                    printfn "%i <-> %i" conn.IdA conn.IdB
                jBoxA.Conn <- Array.append jBoxA.Conn [|conn.IdB|]
                jBoxB.Conn <- Array.append jBoxB.Conn [|conn.IdA|]

                let jBoxBConnections = propagate(jBoxById, jBoxB.Id)
                printfn "%i" jBoxBConnections.Length
                if jBoxBConnections.Length >= juctionBoxes.Length then
                    printfn "A(%A) - B(%A)" [|jBoxA.X; jBoxA.Y; jBoxA.Z|] [|jBoxB.X; jBoxB.Y; jBoxB.Z|]
                    result <- sprintf "%i" (jBoxA.X * jBoxB.X)
                    multipleCircuits <- false
                    
            else
                if verbose then
                    printfn "%i X %i (Dont has Free Conns)" conn.IdA conn.IdB
            connectionCount <- connectionCount + 1
            i <- i + 1
        
        let juctionBoxesInCircuit =
            Array.create juctionBoxes.Length false

        let circuits =
            juctionBoxes
            |> Array.map (fun jBox -> 
                if not juctionBoxesInCircuit.[jBox.Id] then
                    propagate(jBoxById, jBox.Id) 
                    |> Array.map (fun conn ->
                        juctionBoxesInCircuit.[conn] <- true
                        conn)
                else
                    [||]
            )
            |> Array.filter (function
                | [||] -> false
                | _ -> true)
            |> Array.sortByDescending (fun arr -> arr.Length)
            |> Array.map (fun arr -> 
                if verbose then
                    printfn "%A" arr 
                arr)

        let circuitCount =
            circuits
            |> Array.length

        let score =
            let maxIndex = if circuitCount > 2 then 2 else circuitCount - 1
            seq {0..maxIndex}
            |> Seq.fold (fun acc i ->
                acc * int64 circuits.[i].Length
                ) 1L
        
        if verbose then
            printfn "%i circuits" circuitCount
            printfn "%i connections" connectionCount
            printfn "%i score (3 first circuits length multiplied)" score
        result