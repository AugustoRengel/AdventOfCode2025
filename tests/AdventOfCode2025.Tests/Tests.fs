module Tests

open System
open Xunit
open Challenges
open TestsHelpers

[<Fact>]
let ``day01 part1 works for test data`` () =
    // Arrange
    let expected = "3"
    let testFile = pathFromTestDir ["data"; "test_input_01.txt"]
    // Act
    let result = day01.run (
        testFile, 
        false,
        PasswordMethod.Default
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day01 part1 works for data`` () =
    // Arrange
    let expected = "1097" // specific to my challange
    let testFile = pathFromTestDir ["data"; "input_01.txt"]
    // Act
    let result = day01.run (
        testFile, 
        false,
        PasswordMethod.Default
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day01 part2 works for test data`` () =
    // Arrange
    let expected: string = "6"
    let testFile: string = pathFromTestDir ["data"; "test_input_01.txt"]
    // Act
    let result: string = day01.run (
        testFile, 
        false,
        PasswordMethod.PassingBy
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day01 part2 works for data`` () =
    // Arrange
    let expected: string = "7101" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "input_01.txt"]
    // Act
    let result: string = day01.run (
        testFile, 
        false,
        PasswordMethod.PassingBy
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day02 part1 works for test data`` () =
    // Arrange
    let expected: string = "1227775554"
    let testFile: string = pathFromTestDir ["data"; "test_input_02.txt"]
    // Act
    let result: string = day02.run (
        testFile,
        false,
        RepetitionMethod.Twice
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day02 part1 works for data`` () =
    // Arrange
    let expected: string = "23534117921" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "input_02.txt"]
    // Act
    let result: string = day02.run (
        testFile,
        false,
        RepetitionMethod.Twice
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day02 part2 works for data`` () =
    // Arrange
    let expected: string = "31755323497" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "input_02.txt"]
    // Act
    let result: string = day02.run (
        testFile,
        false,
        RepetitionMethod.NTimes
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("33")>]
[<InlineData("111")>]
[<InlineData("446446")>]
[<InlineData("824824824")>]
[<InlineData("2121212121")>]
[<InlineData("1188511885")>]
let ``day02 sequenceDontRepeats`` (input: string) =
    // Arrange
    let expected = $"Id({input}) has sequence of digits repeated twice"
    // Act
    let result = ProductId.isValid(input, RepetitionMethod.NTimes)
    //Assert
    match result with
    | Error msg -> Assert.Equal(expected, msg)    
    | Ok _ -> Assert.Fail "Expecting error"
