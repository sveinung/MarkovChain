namespace Domain

module Calculator =
    type MarkovModel = {
        Name: string
    }

    let public calculateModel x: int =
        x * x
