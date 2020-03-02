module DomainTest.MarkovChainTest

open NUnit.Framework
open Domain
open Domain.MarkovChain

[<Test>]
let Test() =
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
  
    let results = MarkovChain.generateHelper (chain, [0.7; 0.3; 0.8], s0)

    Assert.That(results, Is.EquivalentTo ["1"; "2"])

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
    let result = MarkovChain.nextState (values, 0.1)
    
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
    let result = MarkovChain.nextState (values, 0.4)
    
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
    let result = MarkovChain.nextState (values, 0.6)
    
    Assert.That(result, Is.EqualTo(EndStateValue.State("3")))
