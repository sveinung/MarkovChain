namespace Domain

module Sentence =
    let public parse (text: string): List<List<string>> =
        let parseSingleSentence (sentence: string) =
            sentence
                .Trim()
                .Split [| ' ' |]
                |> Array.toList

        let notEmpty (sentence: list<string>) =
            not (sentence.Equals([""]))

        text.Split [| '.' |]
            |> Array.map parseSingleSentence
            |> Array.filter notEmpty
            |> Array.toList
