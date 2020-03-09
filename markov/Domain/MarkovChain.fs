module Domain.MarkovChain

open System

type StartStateValue =
    | Start
    | State of value : string
    
type EndStateValue =
    | End
    | State of value : string

type Transition = {
    probability: double;
    endState: EndStateValue;
}

type State = {
    state: StartStateValue;
    transitions: Transition list;
}

type MarkovChain = Map<StartStateValue, State>

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

let nextState (transitions: Transition list, randomValue: double): EndStateValue =
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

let public generate(chain: MarkovChain, times: int): string list =
    let random = new Random()
    let randomNumbers: List<double> = [ for _ in 1..times -> double (random.Next(0, 100)) / 100.0 ]
    let startState = chain.Item Start
    generateHelper (chain, randomNumbers, startState)
