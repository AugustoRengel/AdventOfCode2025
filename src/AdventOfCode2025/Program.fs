open Challenges
open Utils

module Dispatcher =
    let run(number: int): string =
        let dataDir = PathHelper.pathFromRoot(["data"])
        match number with
        | 1 
            -> day01.run (
                PathHelper.getDataFile "example_input_01.txt", 
                true,
                PasswordMethod.PassingBy
            )
        | 2 
            -> day02.run (
                PathHelper.getDataFile "input_02.txt", 
                true,
                RepetitionMethod.NTimes
            )
        | _ -> "Modulo não encontrado"

[<EntryPoint>]
let main _ =
    printfn "Qual projeto quer executar? (1 - 12)"
    let number: int = System.Console.ReadLine() |> int
    let result: string = Dispatcher.run number
    printfn "%s" result
    0