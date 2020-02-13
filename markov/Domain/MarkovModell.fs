namespace Domain

module MarkovModell =
    type StartPoint =
        | StartState of value : string
        | Start

    type EndPoint =
        | EndState of value : string
        | End

    type Transision = {
        startPoint: StartPoint;
        endPoint: EndPoint;
    }

    let rec toTransitionsHelper (previousState: string) (sentences: list<string>) =
        match sentences with
        | [] ->
            [{ startPoint = StartState(previousState); endPoint = End }]
        | head :: [] ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: [{ startPoint = StartState(head); endPoint = End }]
        | head :: tail ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let toTransitionsFromSingleChain (sentence: list<string>) =
        match sentence with
        | [] -> []
        | head :: tail ->
            { startPoint = Start; endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let public toTransitions (sentences: list<list<string>>) =
        sentences
            |> List.map toTransitionsFromSingleChain
            |> Seq.concat
            |> List.ofSeq
            |> List.distinct
