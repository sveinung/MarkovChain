module DomainTest.SetningTest

open NUnit.Framework

open Domain

[<Test>]
let Parsing_list_of_sentences() =
    let result = Sentence.parse "hei på deg"
    Assert.That(result, Is.EquivalentTo([["hei"; "på"; "deg"]]))

[<Test>]
let Parsing_two_sentences() =
    let result = Sentence.parse "hei på deg. Her er eg."
    Assert.That(result, Is.EquivalentTo([
        ["hei"; "på"; "deg"];
        ["Her"; "er"; "eg"]
    ]))
