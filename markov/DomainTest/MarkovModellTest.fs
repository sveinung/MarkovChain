module DomainTest.MarkovModellTest

open NUnit.Framework

open Domain
open Domain.MarkovModell

[<Test>]
let ``One state``() =
    let modell = MarkovModell.create [ "1" ]
    Assert.That(modell, Is.EquivalentTo([
        { startPoint = Start;
          endPoint = EndState("1") };

        { startPoint = StartState("1");
          endPoint = End };
    ]))

[<Test>]
let ``Three states``() =
    let modell = MarkovModell.create [
        "1"; "2"; "3"
    ]
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
