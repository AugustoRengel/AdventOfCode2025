namespace Challenges

type Sense =
    | Left
    | Right

type Movement =
    {
        Sense: Sense
        Steps: int
    } with
        override this.ToString (): string = 
            sprintf "%c-%d" (string this.Sense).[0] this.Steps

type PasswordMethod =
    | PassingBy //0x434C49434B
    | Default

type Dial =
    private {
        Position: int
        FirstNumber: int
        LastNumber: int // Not inclusive
        Range: int
        TimesInZero: int
        PasswordMethod: PasswordMethod
        Verbose: bool
    }

module Dial = 

    let create (
        initialPosition: int, 
        firsNumber: int, 
        lastNumber: int, 
        passwordMethod: PasswordMethod,
        verbose: bool
    ) =
        {
            Position = initialPosition;
            FirstNumber = firsNumber;
            LastNumber = lastNumber;
            Range = lastNumber - firsNumber;
            TimesInZero = 0;
            PasswordMethod = passwordMethod;
            Verbose = verbose;
        }

    let getPosition(dial: Dial) = dial.Position
    let getTimesInZero(dial: Dial) = dial.TimesInZero
    let describeMovement(dial: Dial, movement: Movement, newPosition, timesPassedByZero) =
        sprintf "%i\t->\t%s\t->\t%i\t| zeros: %i | timesPassed: %i" 
            dial.Position 
            (movement.ToString()) 
            newPosition 
            dial.TimesInZero 
            timesPassedByZero

    let countPassword(dial: Dial, timesPassedByZero: int) =
        match dial.PasswordMethod with
            |PasswordMethod.PassingBy -> timesPassedByZero
            |PasswordMethod.Default -> if getPosition dial = 0 then 1 else 0

    let moveRight (dial: Dial, steps: int) =
        let result, timesPassedByZero = 
            let newPosition = dial.Position + steps
            let newPositionNormalized = newPosition % dial.Range
            match newPosition with
            | v when v >= dial.LastNumber 
                -> newPositionNormalized, (v / dial.Range)
            | v 
                -> newPositionNormalized, 0
        let counter = countPassword(dial, timesPassedByZero)
        result, counter

    let moveLeft (dial: Dial, steps: int) =
        let result, timesPassedByZero = 
            let counterDefault = 
                match dial.Position = 0 with
                | true -> 0
                | false -> 1
            let newPosition = dial.Position - steps
            let newPositionNormalized = (newPosition % dial.Range) + dial.Range
            match newPosition with
            | v when v <= dial.FirstNumber 
                -> newPositionNormalized % dial.Range, abs(newPosition / dial.Range) + counterDefault
            | v 
                -> newPositionNormalized % dial.Range, 0
        let counter = countPassword(dial, timesPassedByZero)
        result, counter

    let move (dial: Dial, movement: Movement): Dial =
        let newPosition, timesPassedByZero =
            match movement.Sense with
            | Left -> 
                moveLeft(dial, movement.Steps)
            | Right -> 
                moveRight(dial, movement.Steps)
        if dial.Verbose then
            printfn "%s" (describeMovement(dial, movement, newPosition, timesPassedByZero))
        {dial with 
            Position = newPosition
            TimesInZero = dial.TimesInZero + timesPassedByZero}
        
        

module day01 =
    let parseDialMove(dialMove: string): Movement =
        let sense: Sense = 
            match dialMove.[0] with
                | 'L' -> Left
                | 'R' | _ -> Right
        let steps: int =
            dialMove.Substring(1)
            |> int
        {
            Sense=sense 
            Steps=steps
        }

    let run (filePath: string, verbose: bool, method: PasswordMethod) : string =
        let rows = System.IO.File.ReadAllLines(filePath)

        let movements= 
            rows
            |> Array.map parseDialMove
        let dial = Dial.create(50, 0,100, method, verbose)
        let movedDial =
            movements
            |> Array.fold (fun d movement ->
                Dial.move (d, movement)
                ) dial
        string (Dial.getTimesInZero movedDial)