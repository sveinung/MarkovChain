namespace Domain

module MarkovModell =
    type StartPoint<'T> =
        | StartState of value : 'T
        | Start

    type EndPoint<'T> =
        | EndState of value : 'T
        | End

    type Transision<'T> = {
        startPoint: StartPoint<'T>;
        endPoint: EndPoint<'T>;
    }

    let rec toTransitionsHelper<'T> (previousState: 'T) (sentences: list<'T>) =
        match sentences with
        | [] ->
            [{ startPoint = StartState(previousState); endPoint = End }]
        | head :: [] ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: [{ startPoint = StartState(head); endPoint = End }]
        | head :: tail ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let toTransitionsFromSingleChain (sentence: list<'T>) =
        match sentence with
        | [] -> []
        | head :: tail ->
            { startPoint = Start; endPoint = EndState(head) }
            :: toTransitionsHelper head tail

    let public toTransitions (sentences: list<list<'T>>) =
        sentences
            |> List.map toTransitionsFromSingleChain
            |> Seq.concat
            |> List.ofSeq
            |> List.distinct
