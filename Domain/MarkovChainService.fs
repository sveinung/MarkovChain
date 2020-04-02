module Domain.MarkovChainService

open System

open Domain.Analysis
open Domain.Analysis.Analyser
open Domain.MarkovChain

let createModel (text: seq<string>): MarkovChain =
    let concatTransitions (acc: AnalysedTransition list) (elem: AnalysedTransition list): AnalysedTransition list =
        acc @ elem

    text
        |> Seq.map Sentence.parse
        |> Seq.map Analyser.toTransitions
        |> List.ofSeq
        |> List.fold concatTransitions List.empty<AnalysedTransition>
        |> Analyser.toMarkovChain

let nextState (transitions: Transition list, randomValue: double): EndStateValue =
    let rec nextStateHelper (transitions: Transition list, randomValue: double, startOfInterval: double): EndStateValue =
        match transitions with
        | [] -> failwith "Cannot be empty"
        | head :: [] -> head.endState
        | head :: tail ->
            let endOfInterval = startOfInterval + (head.probability)
            if startOfInterval <= randomValue && randomValue < endOfInterval then
                head.endState
            else
                nextStateHelper (tail, randomValue, endOfInterval)
    nextStateHelper (transitions, randomValue, 0.0)

let rec generateHelper(chain: MarkovChain, randomNumbers: double list, currentState: State): string list =
    match randomNumbers with
    | [] -> []
    | head :: [] ->
        let next: EndStateValue = nextState (currentState.transitions, head)
        match next with
        | End -> []
        | State value -> [value]
    | head :: tail ->
        let next: EndStateValue = nextState (currentState.transitions, head)
        match next with
        | End -> []
        | State value ->
            value :: generateHelper (chain, tail, chain.Item (StartStateValue.State value))

let public generate (times: int) (chain: MarkovChain): string list =
    let random = new Random()
    let randomNumbers: List<double> = [ for _ in 1..times -> double (random.Next(0, 100)) / 100.0 ]
    let startState = chain.Item Start
    generateHelper (chain, randomNumbers, startState)
