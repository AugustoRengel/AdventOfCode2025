namespace Challenges

type ProductId = ProductId of string

type RepetitionMethod =
    | Twice 
    | NTimes

// 12 = 2, 3, [4, 6]
// 14 = 2, 7
// 15 = 3, 5
// 16 = 2, [4, 8]

module ProductId =
    let isPrime(number: int) =
        if number < 2 then 
            false
        else
            let bound = int(sqrt(float number))
            seq {2..bound}
            |> Seq.forall (fun n -> number % n <> 0)
    let decompose(number: int, max: int) = 
        let bound = 
            let half = number/2 + 1
            if half < max then
                half
            else
                max
        seq {2 .. bound}
        |> Seq.filter (fun n ->
            number % n = 0
            )
        |> Seq.filter isPrime

    let isValid(productId: string, method) =
        let isInt(s: string) =
            match System.Int64.TryParse s with
            | true, _ -> Ok s
            | false, _ -> Error $"Id({s}) is not an int"

        let sequenceDontRepeats(s: string, method: RepetitionMethod) =
            if s.Length = 1 then
                Ok s
            else
                let components = 
                    match method with
                    | Twice -> 
                        let isOdd = s.Length % 2 = 1
                        if isOdd then
                            seq {}
                        else
                            decompose(s.Length, 2)
                    | NTimes -> 
                        decompose (s.Length, s.Length)
                        |> Seq.append (seq { s.Length })

                components
                |> Seq.forall(fun divisor ->
                    let sequenceSize = s.Length / divisor
                    let sequence = s.Substring(0, sequenceSize)
                    let sequenceReplicated = String.replicate divisor sequence
                    s <> sequenceReplicated
                )
                |> function
                    | true -> Ok s
                    | false -> Error $"Id({s}) has sequence of digits repeated twice"

        productId
        |> isInt
        |> Result.bind (fun x -> sequenceDontRepeats (x, method))


    let create(productId: string, method: RepetitionMethod) =
        match isValid (productId, method) with
        | Ok result -> Ok (ProductId result)
        | Error message-> Error message

module day02 =
    let run(filePath: string, verbose: bool, method: RepetitionMethod) = 
        let rows = System.IO.File.ReadAllLines(filePath) |> Seq.head
        let idRanges =
            rows
            |> fun row -> row.Split ","
            |> Array.map (fun idRange -> idRange.Split "-")
            |> Array.map (fun idRange -> 
                let first = int64 idRange.[0]
                let second = int64 idRange.[1]
                [first..second]
                )

        let sumInvalidIds =
            idRanges
            |> Array.fold (fun acc lst -> 
                lst 
                |> List.fold (fun acc2 id -> 
                    match ProductId.create (string id, method) with
                    | Ok _-> acc2
                    | Error _-> 
                        if verbose then
                            printfn "%i" id
                        acc2 + id
                    ) acc
                )
                0L
        string sumInvalidIds
        // ProductId.decompose (4, 3)
        // |> Seq.iter (printfn "%i")
        // ""