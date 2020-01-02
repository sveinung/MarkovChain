namespace Domain

module Setning =
    let parseSetning (setningString: string) =
        setningString.Split [|' '|]
            |> Array.toList

    let public parseSetningar (tekst: string) =
        let setningar = tekst.Split [|'.'|]
        let result =
            setningar
            |> Array.map parseSetning
            |> Array.toList
        (result)
