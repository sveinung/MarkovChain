module PersistanceTest.MarkovChainFileRepositoryTest

open NUnit.Framework

open Domain.MarkovChain
open Persistance

[<Test>]
let ``Map to JSON``() =
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
    let chain = Map.empty.Add(s0.state, s0).Add(s1.state, s1)
    let result = Mapper.toJson chain
    Assert.That(result, Is.EquivalentTo "{\"1\":[{\"Probability\":0.5,\"EndState\":\"2\"},{\"Probability\":0.5,\"EndState\":\"__END__\"}],\"__START__\":[{\"Probability\":0.5,\"EndState\":\"2\"},{\"Probability\":0.5,\"EndState\":\"1\"}]}")

[<Test>]
let ``Map to objects``() =
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
    let chain = Map.empty.Add(s0.state, s0).Add(s1.state, s1)
    let result = Mapper.fromJson "{\"1\":[{\"Probability\":0.5,\"EndState\":\"2\"},{\"Probability\":0.5,\"EndState\":\"__END__\"}],\"__START__\":[{\"Probability\":0.5,\"EndState\":\"2\"},{\"Probability\":0.5,\"EndState\":\"1\"}]}"
    Assert.That(result, Is.EquivalentTo chain)

