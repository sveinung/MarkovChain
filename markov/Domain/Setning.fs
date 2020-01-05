namespace Domain

module Setning =
    let parseSetning (setningString: string) =
        setningString
            .Trim()
            .Split [| ' ' |]
            |> Array.toList

    let ikkjeTomSetning (setning: list<string>) =
        not (setning.Equals([""]))

    let public parseSetningar (tekst: string) =
        let setningar = tekst.Split [| '.' |]
        let result =
            setningar
            |> Array.map parseSetning
            |> Array.filter ikkjeTomSetning
            |> Array.toList
        (result)
