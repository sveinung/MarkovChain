namespace Domain.Analysis

open Domain.MarkovChain

module Analyser =
    type AnalysedTransition = {
        StartPoint: StartStateValue;
        EndPoint: EndStateValue;
    }

    let public toTransitions (sentences: string list list): AnalysedTransition list =
        let rec toTransitionsHelper (previousState: string) (sentences: string list) =
            match sentences with
            | [] ->
                [{ StartPoint = StartStateValue.State(previousState); EndPoint = End }]
            | head :: [] ->
                { StartPoint = StartStateValue.State(previousState); EndPoint = EndStateValue.State(head) }
                :: [{ StartPoint = StartStateValue.State(head); EndPoint = End }]
            | head :: tail ->
                { StartPoint = StartStateValue.State(previousState); EndPoint = EndStateValue.State(head) }
                :: toTransitionsHelper head tail

        let toTransitionsFromSingleChain (sentence: string list) =
            match sentence with
            | [] -> []
            | head :: tail ->
                { StartPoint = Start; EndPoint = EndStateValue.State(head) }
                :: toTransitionsHelper head tail

        sentences
            |> List.map toTransitionsFromSingleChain
            |> Seq.concat
            |> List.ofSeq
            |> List.distinct

    let public toMarkovChain (allTransitions: AnalysedTransition list): MarkovChain =
        let calculateEdgeWeightsFromState (transitions: AnalysedTransition list): Transition list =
            transitions
                |> List.map (fun t -> t.EndPoint)
                |> Seq.countBy id
                |> Seq.map (fun (state: EndStateValue, count: int) ->
                    { Transition.probability = double count / double transitions.Length;
                      Transition.endState = state })
                |> List.ofSeq

        let toWeightedEdges (startStateValue: StartStateValue, transitions: AnalysedTransition list) =
            let state: State = {
                state = startStateValue;
                transitions = calculateEdgeWeightsFromState transitions
            }
            (startStateValue, state)

        allTransitions
            |> List.groupBy (fun t -> t.StartPoint)
            |> List.map toWeightedEdges
            |> Map.ofList
