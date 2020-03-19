open System
open System.IO

open Domain
open Domain.Transitions

let createModel (sourceFilePath: string): unit =
    let concatTransitions (acc: Transition list) (elem: Transition list): Transition list =
        acc @ elem
    
    printfn "Creating model from %s ..." sourceFilePath
    File.ReadLines sourceFilePath
        |> Seq.map Sentence.parse
        |> Seq.map Transitions.toTransitions
        |> List.ofSeq
        |> List.fold concatTransitions List.empty<Transition>
        |> Transitions.toMarkovChain
        |> MarkovChainFileRepository.toJson
        |> (fun x -> Console.WriteLine ("--- " + x))
        |> ignore
    ()

[<EntryPoint>]
let main args =
    match args with
    | [|"create-model" ; sourceFilePath |] ->
        createModel sourceFilePath
        0
    | _ ->
        Console.WriteLine ("Usage:
            create-model <text file>")
        0
