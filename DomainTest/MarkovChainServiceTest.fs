module DomainTest.MarkovChainTest

open NUnit.Framework
open Domain
open Domain.MarkovChain

let fakeRandom (fakeRandomNumbers: double list): unit -> double =
    let mutable index = 0
    fun () ->
        let result = fakeRandomNumbers.Item index
        index <- index + 1
        result

let s0: State =
    { state = StartStateValue.Start;
      transitions = [
          { probability = 0.5;
            endState = EndStateValue.State("2") };
          { probability = 0.5;
            endState = EndStateValue.State("1") }
      ]
    }
let s1: State =
    { state = StartStateValue.State("1");
      transitions = [
          { probability = 0.5;
            endState = EndStateValue.State("2") };
          { probability = 0.5;
            endState = EndStateValue.End }
      ]
    }
let s2: State =
    { state = StartStateValue.State("2");
      transitions = [
          { probability = 0.5;
            endState = EndStateValue.State("2") };
          { probability = 0.5;
            endState = EndStateValue.End }
      ]
    }
let chain = Map.empty.Add(s0.state, s0).Add(s1.state, s1).Add(s2.state, s2)

[<Test>]
let Test() =
    let results = MarkovChainService.generateSentences (chain, fakeRandom [0.7; 0.6; 0.3; 0.8], 2, s0)

    Assert.That(results, Is.EquivalentTo ["1"; "2"])

[<Test>]
let Test_2() =
    let results = MarkovChainService.generateSentences (chain, fakeRandom ([0.7; 0.3; 0.3; 0.8; 0.7; 0.3; 0.3; 0.8]), 2, s0)

    Assert.That(results, Is.EquivalentTo ["1"; "2"; "2"; "1"; "2"; "2"])

[<Test>]
let Should_select_the_first() =
    let values = [
        { probability = 0.2;
          endState = EndStateValue.State("1") };
        { probability = 0.3;
          endState = EndStateValue.State("2") };
        { probability = 0.5;
          endState = EndStateValue.State("3") }
    ]
    let result = MarkovChainService.nextState (values, 0.1)
    
    Assert.That(result, Is.EqualTo(EndStateValue.State("1")))

[<Test>]
let Should_select_the_second() =
    let values = [
        { probability = 0.2;
          endState = EndStateValue.State("1") };
        { probability = 0.3;
          endState = EndStateValue.State("2") };
        { probability = 0.5;
          endState = EndStateValue.State("3") }
    ]
    let result = MarkovChainService.nextState (values, 0.4)
    
    Assert.That(result, Is.EqualTo(EndStateValue.State("2")))

[<Test>]
let Should_select_the_third() =
    let values = [
        { probability = 0.2;
          endState = EndStateValue.State("1") };
        { probability = 0.3;
          endState = EndStateValue.State("2") };
        { probability = 0.5;
          endState = EndStateValue.State("3") }
    ]
    let result = MarkovChainService.nextState (values, 0.6)
    
    Assert.That(result, Is.EqualTo(EndStateValue.State("3")))
