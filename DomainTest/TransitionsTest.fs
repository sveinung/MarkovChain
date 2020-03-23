module DomainTest.MarkovModellTest

open NUnit.Framework

open Domain
open Domain.Transitions
open Domain.MarkovChain

[<Test>]
let ``One state``() =
    let modell = Transitions.toTransitions [[ "1" ]]
    Assert.That(modell, Is.EquivalentTo([
        { startPoint = Start;
          endPoint = EndStateValue.State("1") };

        { startPoint = StartStateValue.State("1");
          endPoint = End };
    ]))

[<Test>]
let ``Three states``() =
    let modell = Transitions.toTransitions [["1"; "2"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndStateValue.State("1") };

        { startPoint = StartStateValue.State("1");
          endPoint = EndStateValue.State("2") };

        { startPoint = StartStateValue.State("2");
          endPoint = EndStateValue.State("3") };

        { startPoint = StartStateValue.State("3");
          endPoint = End };
    ])

[<Test>]
let ``Two chains without overlap``() =
    let modell = Transitions.toTransitions [["1"; "2"]; ["3"; "4"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndStateValue.State("1") };

        { startPoint = StartStateValue.State("1");
          endPoint = EndStateValue.State("2") };
        
        { startPoint = StartStateValue.State("2");
          endPoint = End };
        
        { startPoint = Start;
          endPoint = EndStateValue.State("3") };

        { startPoint = StartStateValue.State("3");
          endPoint = EndStateValue.State("4") };

        { startPoint = StartStateValue.State("4");
          endPoint = End };
    ])

[<Test>]
let ``One chain branching into two``() =
    let modell = Transitions.toTransitions [["1"; "2"]; ["1"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndStateValue.State("1") };

        { startPoint = StartStateValue.State("1");
          endPoint = EndStateValue.State("2") };

        { startPoint = StartStateValue.State("2");
          endPoint = End };

        { startPoint = StartStateValue.State("1");
          endPoint = EndStateValue.State("3") };

        { startPoint = StartStateValue.State("3");
          endPoint = End };
    ])

[<Test>]
let ``Chain with one state``() =
    let markovChain = Transitions.toMarkovChain [
        { startPoint = Start;
            endPoint = EndStateValue.State("1") };
        { startPoint = Start;
            endPoint = EndStateValue.State("2") };
        { startPoint = StartStateValue.State("1");
            endPoint = EndStateValue.State("2") };
    ]
    let start = markovChain.Item MarkovChain.Start
    Assert.That (start.transitions, Is.EquivalentTo [
      { MarkovChain.Transition.probability = 0.5;
        MarkovChain.Transition.endState = MarkovChain.EndStateValue.State "1";
      };
      { MarkovChain.Transition.probability = 0.5;
        MarkovChain.Transition.endState = MarkovChain.EndStateValue.State "2";
      }
    ])
    let state1 = markovChain.Item (MarkovChain.StartStateValue.State "1")
    Assert.That (state1.transitions, Is.EquivalentTo [
      { MarkovChain.Transition.probability = 1.0;
        MarkovChain.Transition.endState = MarkovChain.EndStateValue.State "2";
      }
    ])
