namespace Challenges

module day03 = 
    let findLargestDigit(s: string) = 
        let index, maxValue =
            s.ToCharArray()
            |> Array.map System.Char.GetNumericValue
            |> Array.map int
            |> Array.indexed
            |> Array.maxBy snd
        index, maxValue

    let rec findLargestJoltage(bank: string, digits: int) =
        let segment = bank.Substring(0, bank.Length + 1 - digits)
        let index, maxValue = findLargestDigit segment

        if digits > 1 then
            let newBank = bank.Substring(index + 1, bank.Length - (index + 1))
            sprintf "%i%s" maxValue (findLargestJoltage(newBank, digits -  1))
        else
            sprintf "%i" maxValue

    let run (filePath: string, digits: int, verbose: bool) = 
        let rows = System.IO.File.ReadAllLines(filePath)
        rows
        |> Array.map(fun s -> findLargestJoltage(s, digits))
        |> Array.map (fun s -> 
            if verbose then
                printfn "+%s" s
            s)
        |> Array.map int64
        |> Array.sum
        |> sprintf "%i"