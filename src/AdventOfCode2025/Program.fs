open Challenges
open Utils

module Dispatcher =
    let run(number: int): string =
        match number with
        | 1 
            -> day01.run (
                PathHelper.getDataFile "input_01.txt", 
                true,
                PasswordMethod.PassingBy
            )
        | 2 
            -> day02.run (
                PathHelper.getDataFile "input_02.txt", 
                true,
                RepetitionMethod.NTimes
            )
        | 3 
            ->  printfn "Quantos digitos de Joltagem?"
                let digits: int = System.Console.ReadLine() |> int
                day03.run (
                PathHelper.getDataFile "input_03.txt", 
                digits,
                true
            )
        | 4 
            ->  day04.run (
                    PathHelper.getDataFile "input_04.txt",
                    true,
                    true
                )
        | _ -> "Modulo não encontrado"

[<EntryPoint>]
let main _ =
    printfn "Qual projeto quer executar? (1 - 12)"
    let number: int = System.Console.ReadLine() |> int
    let result: string = Dispatcher.run number
    printfn "%s" result
    0