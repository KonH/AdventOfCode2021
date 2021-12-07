module AdventOfCode2021.Day05Part2

open System
open System.IO

let run() =
    Console.WriteLine "Day 05 Part2"
    let inputs =
        File.ReadAllLines "Day05Part1.txt"
    Console.WriteLine $"Found {inputs.Length} inputs"
    let lines =
        inputs
        |> Array.map (fun input ->
            let parts = input.Split([|' '; ','|])
            let x1, y1, x2, y2 = parts.[0], parts.[1], parts.[3], parts.[4]
            let p1 = (Int32.Parse x1, Int32.Parse y1)
            let p2 = (Int32.Parse x2, Int32.Parse y2)
            p1, p2)
    let detectLinePoints line =
        let (x1, y1), (x2, y2) = line
        let normalized (d: int) =
            Math.Sign d
        let rec create p1 p2 d ps =
            let (x1, y1), (x2, y2), (dx, dy) = p1, p2, d
            if x1 = x2 && y1 = y2
            then (p2 :: ps)
            else create (x1 + dx, y1 + dy) p2 d (p1 :: ps)
        let dx = normalized (x2 - x1)
        let dy = normalized (y2 - y1)
        create (x1, y1) (x2, y2) (dx, dy) []
    let fillPoints state points =
        points
        |> List.fold (fun state point ->
            state
            |> Map.change point (fun oldValue ->
            match oldValue with
            | Some v -> Some (v + 1)
            | None _ -> Some 1)) state
    let fill state line =
        fillPoints state (detectLinePoints line)
    let world = lines |> Array.fold fill Map.empty
    let targets = world |> Map.filter (fun _ v -> v > 1)
    let count = targets |> Map.count
    Console.WriteLine $"Result is {count}"