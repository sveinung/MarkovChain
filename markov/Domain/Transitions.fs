namespace Domain

open Domain.MarkovChain

module Transitions =
    type Transition = {
        startPoint: StartStateValue;
        endPoint: EndStateValue;
    }

    let public toTransitions (sentences: string list list): Transition list =
        let rec toTransitionsHelper (previousState: string) (sentences: string list) =
            match sentences with
            | [] ->
                [{ startPoint = StartStateValue.State(previousState); endPoint = End }]
            | head :: [] ->
                { startPoint = StartStateValue.State(previousState); endPoint = EndStateValue.State(head) }
                :: [{ startPoint = StartStateValue.State(head); endPoint = End }]
            | head :: tail ->
                { startPoint = StartStateValue.State(previousState); endPoint = EndStateValue.State(head) }
                :: toTransitionsHelper head tail

        let toTransitionsFromSingleChain (sentence: string list) =
            match sentence with
            | [] -> []
            | head :: tail ->
                { startPoint = Start; endPoint = EndStateValue.State(head) }
                :: toTransitionsHelper head tail

        sentences
            |> List.map toTransitionsFromSingleChain
            |> Seq.concat
            |> List.ofSeq
            |> List.distinct

    let public toMarkovChain (allTransitions: Transition list): MarkovChain =
        let calculateEdgeWeightsFromState (transitions: Transition list): MarkovChain.Transition list =
            transitions
                |> List.map (fun t -> t.endPoint)
                |> Seq.countBy id
                |> Seq.map (fun (state: EndStateValue, count: int) ->
                    { MarkovChain.Transition.probability = double count / double transitions.Length;
                      MarkovChain.Transition.endState = state })
                |> List.ofSeq

        let toWeightedEdges (startStateValue: StartStateValue, transitions: Transition list) =
            let state: State = {
                state = startStateValue;
                transitions = calculateEdgeWeightsFromState transitions
            }
            (startStateValue, state)

        allTransitions
            |> List.groupBy (fun t -> t.startPoint)
            |> List.map toWeightedEdges
            |> Map.ofList
