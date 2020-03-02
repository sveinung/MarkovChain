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
    transitions: list<Transition>;
}

type MarkovChain = Map<StartStateValue, State>

let rec nextStateHelper (transitions: list<Transition>, randomValue: double, startOfInterval: double): EndStateValue =
    match transitions with
    | [] -> failwith "Cannot be empty"
    | head :: [] -> head.endState
    | head :: tail ->
        let endOfInterval = startOfInterval + (head.probability)
        if startOfInterval <= randomValue && randomValue < endOfInterval then
            head.endState
        else
            nextStateHelper (tail, randomValue, endOfInterval)

let nextState (transitions: list<Transition>, randomValue: double): EndStateValue =
    nextStateHelper (transitions, randomValue, 0.0)

let rec public generate2(chain: MarkovChain, randomNumbers: List<double>, currentState: State): list<string> =
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
            value :: generate2 (chain, tail, chain.Item (StartStateValue.State value))

let public generate(chain: MarkovChain, times: int): list<string> =
    let random = new Random()
    let randomNumbers: List<double> = [ for _ in 1..times -> double (random.Next(0, 100)) / 100.0 ]
    let startState = chain.Item Start
    generate2 (chain, randomNumbers, startState)
