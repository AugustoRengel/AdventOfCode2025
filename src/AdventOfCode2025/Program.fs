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
        | _ -> "Modulo não encontrado"

[<EntryPoint>]
let main _ =
    printf "Qual projeto quer executar? (1 - 12)"
    let number: int = System.Console.ReadLine() |> int
    let result: string = Dispatcher.run number
    printf "%s" result
    0