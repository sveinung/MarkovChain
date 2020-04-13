open System
open System.IO

open Domain
open Persistance

let createModel (sourceFilePath: string): unit =
    printfn "Creating model from %s ..." sourceFilePath
    File.ReadLines sourceFilePath
        |> MarkovChainService.createModel
        |> FileRepository.saveModel sourceFilePath
    ()

let generate (modelFilePath: string) (times: int): unit =
    let printSentence (sentence: string list) =
        let sentenceString = String.concat " " sentence
        printfn "%s " sentenceString

    File.ReadAllText modelFilePath
        |> Mapper.fromJson
        |> MarkovChainService.generate times
        |> List.iter printSentence
    ()

[<EntryPoint>]
let main args =
    match args with
    | [|"create-model" ; sourceFilePath |] ->
        createModel sourceFilePath

    | [|"generate" ; sourceFilePath ; times |] ->
        generate sourceFilePath (int times)

    | _ ->
        Console.WriteLine ("Usage:
            create-model <text file>")

    0
