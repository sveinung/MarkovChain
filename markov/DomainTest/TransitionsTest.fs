module DomainTest.MarkovModellTest

open NUnit.Framework

open Domain
open Domain.Transitions

[<Test>]
let ``One state``() =
    let modell = Transitions.toTransitions [[ "1" ]]
    Assert.That(modell, Is.EquivalentTo([
        { startPoint = Start;
          endPoint = EndState("1") };

        { startPoint = StartState("1");
          endPoint = End };
    ]))

[<Test>]
let ``Three states``() =
    let modell = Transitions.toTransitions [["1"; "2"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndState("1") };

        { startPoint = StartState("1");
          endPoint = EndState("2") };

        { startPoint = StartState("2");
          endPoint = EndState("3") };

        { startPoint = StartState("3");
          endPoint = End };
    ])

[<Test>]
let ``Two chains without overlap``() =
    let modell = Transitions.toTransitions [["1"; "2"]; ["3"; "4"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndState("1") };

        { startPoint = StartState("1");
          endPoint = EndState("2") };
        
        { startPoint = StartState("2");
          endPoint = End };
        
        { startPoint = Start;
          endPoint = EndState("3") };

        { startPoint = StartState("3");
          endPoint = EndState("4") };

        { startPoint = StartState("4");
          endPoint = End };
    ])

[<Test>]
let ``One chain branching into two``() =
    let modell = Transitions.toTransitions [["1"; "2"]; ["1"; "3"]]
    Assert.That (modell, Is.EquivalentTo [
        { startPoint = Start;
          endPoint = EndState("1") };

        { startPoint = StartState("1");
          endPoint = EndState("2") };

        { startPoint = StartState("2");
          endPoint = End };

        { startPoint = StartState("1");
          endPoint = EndState("3") };

        { startPoint = StartState("3");
          endPoint = End };
    ])
