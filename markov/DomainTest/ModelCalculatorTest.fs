module DomainTest

open NUnit.Framework

open Domain.Calculator

[<SetUp>]
let Setup() =
    ()

[<Test>]
let Test1() =
    let result = calculateModel 5
    Assert.That(result, Is.EqualTo(25))
