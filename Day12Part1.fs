module AdventOfCode2021.Day12Part1

open System
open System.IO

type CaveType = Small | Big

let run() =
    Console.WriteLine "Day 12 Part1"
    let inputs =
        File.ReadAllLines "Day12Part1.txt"

    let detectCaveType (str: string) =
        if Char.IsUpper str.[0] then Big else Small

    let makeCave str =
        (str, detectCaveType str)

    let connections =
        inputs
        |> Array.map (fun line ->
            let parts = line.Split '-'
            let startCave = parts.[0]
            let endCave = parts.[1]
            (makeCave startCave, makeCave endCave))
        |> List.ofArray

    let getConnections cave =
        connections
        |> List.filter (fun conn ->
            let (fromCave, _), (toCave, _) = conn
            (fromCave = cave) || (toCave = cave))
        |> List.map (fun conn ->
            let (fromCave, fromType), (toCave, toType) = conn
            if (fromCave = cave) then (toCave, toType) else (fromCave, fromType))

    let mutable paths = []

    let rec constructPath path =
        let head, _ = List.head path
        if head = "end"
        then
            paths <- path :: paths
        else
            let connections = getConnections head
            let nonDuplicatedConnections =
                connections
                |> List.filter (fun cave ->
                    let _, size = cave
                    if size = Big then true else not (path |> List.contains cave))
            nonDuplicatedConnections
            |> List.map (fun cave -> constructPath (cave :: path)) |> ignore

    let writeAsText ps =
        let texts =
            ps
            |> List.map (fun p ->
                let revNamesOnly =
                    p
                    |> List.map (fun c ->
                        let name, _ = c
                        name)
                    |> List.rev
                String.Join(',', revNamesOnly))
            |> List.sort
        for t in texts do
            Console.WriteLine t

    constructPath [("start", Small)]

    writeAsText paths

    Console.WriteLine $"Result is {List.length paths}"