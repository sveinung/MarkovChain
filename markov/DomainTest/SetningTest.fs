module DomainTest

open NUnit.Framework

open Domain

[<SetUp>]
let Setup() =
    ()

[<Test>]
let Parsing_av_enkeltsetningar() =
    let result = Setning.parseSetning "hei på deg"
    Assert.That(result, Is.EqualTo(["hei"; "på"; "deg"]))

[<Test>]
let Parsing_av_setningar() =
    let result = Setning.parseSetningar "hei på deg"
    Assert.That(result, Is.EqualTo([["hei"; "på"; "deg"]]))

