module MarkovChainFileRepository

open Newtonsoft.Json

open Domain.MarkovChain

type TransitionDto = {
    Probability: double;
    EndState: string;
}

let toJson (markovChain: MarkovChain): string =
    let mapTransition (transition: Transition): TransitionDto =
        let endStateString =
            match transition.endState with
            | End -> "__END__"
            | EndStateValue.State value -> value

        { Probability = transition.probability;
          EndState = endStateString}

    let mapState ((_, state): StartStateValue * State): string * TransitionDto list =
        let stateString =
            match state.state with
            | Start -> "__START__"
            | StartStateValue.State value -> value

        let transitionsJson =
            state.transitions
                |> List.map mapTransition

        (stateString, transitionsJson)

    markovChain
        |> Map.toList
        |> List.map mapState
        |> Map.ofList
        |> JsonConvert.SerializeObject
