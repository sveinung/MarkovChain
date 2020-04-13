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

let rec generateSentences (chain: MarkovChain, randomNumber: unit -> double, times: int): string list list =
    let rec generateSentence (currentState: State) =
        let number: double = randomNumber ()
        let next: EndStateValue = nextState (currentState.transitions, number)
        match next with
        | End ->
            []
        | State value ->
            value :: generateSentence (chain.Item (StartStateValue.State value))

    if times = 0 then
        []
    else
        let startState = chain.Item Start
        [ generateSentence (startState) ] @ generateSentences (chain, randomNumber, times - 1)

let public generate (times: int) (chain: MarkovChain): string list list =
    let random = new Random()

    let randomNumber () =
        double (random.Next(0, 100)) / 100.0

    generateSentences (chain, randomNumber, times)
