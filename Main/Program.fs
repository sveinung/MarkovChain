open System
open System.IO

open Domain
open Domain.Analysis
open Domain.Analysis.Analyser

let createModel (sourceFilePath: string): unit =
    let concatTransitions (acc: AnalysedTransition list) (elem: AnalysedTransition list): AnalysedTransition list =
        acc @ elem

    let save json =
        let fileName = String.Format ("{0}.markovmodel.json", sourceFilePath)
        File.WriteAllText (fileName, json)
        Console.WriteLine ("Model written to " + fileName)

    printfn "Creating model from %s ..." sourceFilePath
    File.ReadLines sourceFilePath
        |> Seq.map Sentence.parse
        |> Seq.map Analyser.toTransitions
        |> List.ofSeq
        |> List.fold concatTransitions List.empty<AnalysedTransition>
        |> Analyser.toMarkovChain
        |> MarkovChainFileRepository.toJson
        |> save
    ()

let generate (modelFilePath: string) (times: int): unit =
    let printState (state: string) =
        printf "%s " state

    File.ReadAllText modelFilePath
        |> MarkovChainFileRepository.fromJson
        |> MarkovChainService.generate times
        |> List.iter printState
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
