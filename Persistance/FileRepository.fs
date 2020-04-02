namespace Persistance

open System
open System.IO

open Domain.MarkovChain
open Persistance

module FileRepository =
    let saveModel (sourceFilePath: string) (markovChain: MarkovChain): unit =
        let save (json: string) =
            let fileName = String.Format ("{0}.markovmodel.json", sourceFilePath)
            File.WriteAllText (fileName, json)
            Console.WriteLine ("Model written to " + fileName)

        markovChain
            |> Mapper.toJson
            |> save
