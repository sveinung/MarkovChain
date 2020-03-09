namespace Domain

module Sentence =
    let parseSingleSentence (sentence: string) =
        sentence
            .Trim()
            .Split [| ' ' |]
            |> Array.toList

    let notEmpty (sentence: list<string>) =
        not (sentence.Equals([""]))

    let public parse (text: string): List<List<string>> =
        let sentences = text.Split [| '.' |]
        let result =
            sentences
            |> Array.map parseSingleSentence
            |> Array.filter notEmpty
            |> Array.toList
        (result)
