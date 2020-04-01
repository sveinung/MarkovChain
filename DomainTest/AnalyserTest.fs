module DomainTest.MarkovModellTest

open NUnit.Framework

open Domain
open Domain.Analysis
open Domain.Analysis.Analyser
open Domain.MarkovChain

[<Test>]
let ``One state``() =
    let modell = Analyser.toTransitions [[ "1" ]]
    Assert.That(modell, Is.EquivalentTo([
        { StartPoint = Start;
          EndPoint = EndStateValue.State("1") };

        { StartPoint = StartStateValue.State("1");
          EndPoint = End };
    ]))

[<Test>]
let ``Three states``() =
    let modell = Analyser.toTransitions [["1"; "2"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { StartPoint = Start;
          EndPoint = EndStateValue.State("1") };

        { StartPoint = StartStateValue.State("1");
          EndPoint = EndStateValue.State("2") };

        { StartPoint = StartStateValue.State("2");
          EndPoint = EndStateValue.State("3") };

        { StartPoint = StartStateValue.State("3");
          EndPoint = End };
    ])

[<Test>]
let ``Two chains without overlap``() =
    let modell = Analyser.toTransitions [["1"; "2"]; ["3"; "4"]]
    Assert.That (modell, Is.EquivalentTo [
        { StartPoint = Start;
          EndPoint = EndStateValue.State("1") };

        { StartPoint = StartStateValue.State("1");
          EndPoint = EndStateValue.State("2") };

        { StartPoint = StartStateValue.State("2");
          EndPoint = End };

        { StartPoint = Start;
          EndPoint = EndStateValue.State("3") };

        { StartPoint = StartStateValue.State("3");
          EndPoint = EndStateValue.State("4") };

        { StartPoint = StartStateValue.State("4");
          EndPoint = End };
    ])

[<Test>]
let ``One chain branching into two``() =
    let modell = Analyser.toTransitions [["1"; "2"]; ["1"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { StartPoint = Start;
          EndPoint = EndStateValue.State("1") };

        { StartPoint = StartStateValue.State("1");
          EndPoint = EndStateValue.State("2") };

        { StartPoint = StartStateValue.State("2");
          EndPoint = End };

        { StartPoint = StartStateValue.State("1");
          EndPoint = EndStateValue.State("3") };

        { StartPoint = StartStateValue.State("3");
          EndPoint = End };
    ])

[<Test>]
let ``Chain with one state``() =
    let markovChain = Analyser.toMarkovChain [
        { StartPoint = Start;
            EndPoint = EndStateValue.State("1") };
        { StartPoint = Start;
            EndPoint = EndStateValue.State("2") };
        { StartPoint = StartStateValue.State("1");
            EndPoint = EndStateValue.State("2") };
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
