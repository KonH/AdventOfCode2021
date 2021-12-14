module AdventOfCode2021.Day10Part2

open System
open System.IO

let run() =
    Console.WriteLine "Day 10 Part2"
    let inputs =
        File.ReadAllLines "Day10Part1.txt"
    let possibleChars = [
        ('(', ')');
        ('[', ']');
        ('{', '}');
        ('<', '>');
    ]
    let scores = [ ')', 1; ']', 2; '}', 3; '>', 4; ] |> Map.ofList
    let findOpenPair c =
        possibleChars |> List.find (fun cs ->
            let openCh, _ = cs
            openCh = c)
    let tryFindOpenPair c =
        possibleChars |> List.tryFind (fun cs ->
            let openCh, _ = cs
            openCh = c)
    let parsedInputs =
        inputs |> Array.map (fun input ->
            let mutable state = []
            let lineChars = input.ToCharArray()
            let mutable isCorrupted = false
            for c in lineChars do
                if isCorrupted = false then
                    match state with
                    | head :: tail ->
                        match tryFindOpenPair c with
                        | Some op -> state <- op :: state
                        | _ ->
                            let _, expectedCloseCh = head
                            let isValidClosedCh = c = expectedCloseCh
                            if isValidClosedCh
                            then
                                state <- tail
                            else
                                isCorrupted <- true
                                ()
                    | _ ->
                        state <- [findOpenPair c]
                else ()
            (isCorrupted, state))
        |> Array.filter (fun pi ->
            let isCorrupted, _ = pi
            isCorrupted = false)
    let scores = parsedInputs |> Array.map (fun pi ->
        let _, state = pi
        state |> List.fold (fun s it ->
            let _, closeCh = it
            s * 5L + int64(scores |> Map.find closeCh)) 0L)
    let sortedScores = scores |> Array.sort
    let targetScores = sortedScores |> Array.skip ((scores |> Array.length) / 2) |> Array.take 1
    Console.WriteLine $"Result is {targetScores.[0]}"