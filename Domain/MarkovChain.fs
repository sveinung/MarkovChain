namespace Domain

module MarkovChain =
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
