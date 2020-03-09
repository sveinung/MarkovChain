namespace Domain

open Domain.MarkovChain

module Transitions =
    type StartPoint =
        | StartState of value : string
        | Start

    type EndPoint =
        | EndState of value : string
        | End

    type Transition = {
        startPoint: StartPoint;
        endPoint: EndPoint;
    }

    let rec toTransitionsHelper (previousState: string) (sentences: List<string>) =
        match sentences with
        | [] ->
            [{ startPoint = StartState(previousState); endPoint = End }]
        | head :: [] ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: [{ startPoint = StartState(head); endPoint = End }]
        | head :: tail ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let toTransitionsFromSingleChain (sentence: List<string>) =
        match sentence with
        | [] -> []
        | head :: tail ->
            { startPoint = Start; endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let public toTransitions (sentences: list<list<string>>): List<Transition> =
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
                |> Seq.map (fun (state: EndPoint, count: int) ->
                    let endState =
                        match state with
                        | End -> EndStateValue.End
                        | EndState value -> EndStateValue.State value
                    { MarkovChain.Transition.probability = double count / double transitions.Length;
                      MarkovChain.Transition.endState = endState })
                |> List.ofSeq

        let toWeightedEdges (startPoint: StartPoint, transitions: Transition list) =
            let startStateValue =
                match startPoint with
                | Start -> StartStateValue.Start
                | StartState value -> StartStateValue.State value
            let state: State = {
                state = startStateValue;
                transitions = calculateEdgeWeightsFromState transitions
            }
            (startStateValue, state)

        allTransitions
            |> List.groupBy (fun t -> t.startPoint)
            |> List.map toWeightedEdges
            |> Map.ofList
