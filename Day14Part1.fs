module AdventOfCode2021.Day14Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 14 Part1"
    let inputs =
        File.ReadAllLines "Day14Part1.txt"

    let mutable template = inputs[0].ToCharArray() |> List.ofArray

    let insertionRules =
        inputs
        |> Array.skip 2
        |> Array.map (fun l ->
            let parts = l.Split " -> "
            (parts[0].[0], parts[0][1], parts[1][0]))

    let makeInsert state =
        let pairs = state |> List.pairwise
        let insertedPairs =
            pairs
            |> List.mapi (fun i (fst, snd) ->
                let res =
                    match insertionRules |> Array.tryFind (fun (f, s, _) -> f = fst && s = snd) with
                    | Some (_, _, i) -> [fst; i; snd]
                    | None -> [fst; snd]
                if i > 0 then (res |> List.skip 1) else res)
        insertedPairs |> List.concat

    let write (state: char list) =
        Console.WriteLine (state |> List.length)

    for i in [1..10] do
        Console.WriteLine $"Step #{i}"
        template <- makeInsert template
        write template
        Console.WriteLine ""

    let counts = template |> List.countBy id |> List.sortBy snd
    let _, mostCommon = counts |> List.last
    let _, leastCommon = counts |> List.head
    let result = mostCommon - leastCommon
    Console.WriteLine result