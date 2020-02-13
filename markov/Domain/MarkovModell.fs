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

    let rec createHelper (previousState: string) (sentences: list<string>) =
        match sentences with
        | [] ->
            [{ startPoint = StartState(previousState); endPoint = End }]
        | head :: [] ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: [{ startPoint = StartState(head); endPoint = End }]
        | head :: tail ->
            { startPoint = StartState(previousState); endPoint = EndState(head) }
            :: createHelper head tail

    let public create (sentences: list<string>) =
        match sentences with
        | [] -> []
        | head :: tail ->
            { startPoint = Start; endPoint = EndState(head) }
            :: createHelper head tail
