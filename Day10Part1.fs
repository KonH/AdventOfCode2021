module AdventOfCode2021.Day10Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 10 Part1"
    let inputs =
        File.ReadAllLines "Day10Part1.txt"
    let possibleChars = [
        ('(', ')');
        ('[', ']');
        ('{', '}');
        ('<', '>');
    ]
    let scores = [ ')', 3; ']', 57; '}', 1197; '>', 25137; ] |> Map.ofList
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
            let mutable score = 0
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
                                score <- scores |> Map.find c
                                ()
                    | _ ->
                        state <- [findOpenPair c]
                else ()
            (isCorrupted, score))
    let sum = parsedInputs |> Array.sumBy (fun x ->
        let _, score = x
        score)
    Console.WriteLine $"Result is {sum}"