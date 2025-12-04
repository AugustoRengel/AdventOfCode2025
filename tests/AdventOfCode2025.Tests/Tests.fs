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
    let expected = "1097"
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
    let expected: string = "7101"
    let testFile: string = pathFromTestDir ["data"; "input_01.txt"]
    // Act
    let result: string = day01.run (
        testFile, 
        false,
        PasswordMethod.PassingBy
    )
    //Assert
    Assert.Equal(expected, result)
