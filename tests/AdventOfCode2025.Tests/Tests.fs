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

[<Fact>]
let ``day03 part1 works for test data`` () =
    // Arrange
    let expected: string = "357" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "test_input_03.txt"]
    // Act
    let result: string = day03.run (
        testFile,
        2,
        false
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day03 part1 works for data`` () =
    // Arrange
    let expected: string = "17435" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "input_03.txt"]
    // Act
    let result: string = day03.run (
        testFile,
        2,
        false
    )
    //Assert
    Assert.Equal(expected, result)

[<Fact>]
let ``day03 part2 works for data`` () =
    // Arrange
    let expected: string = "172886048065379" // specific to my challange
    let testFile: string = pathFromTestDir ["data"; "input_03.txt"]
    // Act
    let result: string = day03.run (
        testFile,
        12,
        false
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_04.txt", "1489")>]
[<InlineData("test_input_04.txt", "13")>]
let ``day04 part1 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day04.run (
        testFile,
        false,
        false
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_04.txt", "8890")>]
[<InlineData("test_input_04.txt", "43")>]
let ``day04 part2 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day04.run (
        testFile,
        false, 
        true
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_05.txt", "563")>]
[<InlineData("test_input_05.txt", "3")>]
let ``day05 part1 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day05.run (
        testFile,
        false,
        false
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_05.txt", "338693411431456")>]
[<InlineData("test_input_05.txt", "14")>]
let ``day05 part2 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day05.run (
        testFile,
        false,
        true
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_06.txt", "5227286044585")>]
[<InlineData("test_input_06.txt", "4277556")>]
let ``day06 part1 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day06.run (
        testFile,
        false,
        ReadMethod.Human
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_06.txt", "10227753257799")>]
[<InlineData("test_input_06.txt", "3263827")>]
let ``day06 part2 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day06.run (
        testFile,
        false,
        ReadMethod.Cephalopod
    )
    //Assert
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("input_07.txt", "3806264447357")>]
[<InlineData("test_input_07.txt", "40")>]
let ``day07 part2 works for data`` (fileName: string, expected: string) =
    // Arrange
    let testFile: string = pathFromTestDir ["data"; fileName]
    // Act
    let result: string = day07.run (
        testFile,
        false
    )
    //Assert
    Assert.Equal(sprintf "timelines: %s" expected, result)