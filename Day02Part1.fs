module AdventOfCode2021.Day02Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 02 Part1"
    let inputs =
        File.ReadAllLines "Day02Part1.txt"
        |> Array.map (fun line ->
            let lineParts = line.Split(' ')
            let command = lineParts.[0]
            let value = Int32.Parse lineParts.[1]
            (command, value))
    Console.WriteLine $"Found {inputs.Length} inputs"
    let state =
        inputs
        |> Array.map (fun cmd ->
            let choose, value = cmd
            match choose with
            | "forward" -> (0, value)
            | "down" -> (value, 0)
            | "up" -> (-value, 0)
            | _ -> (0, 0)
            )
        |> Array.fold (fun x y ->
            let x1, x2 = x
            let y1, y2 = y
            (x1 + y1, x2 + y2)) (0, 0)
    let x, y = state
    Console.WriteLine $"Final position is ({x}, {y})"
    Console.WriteLine $"Result is {x * y}"
    Console.WriteLine ""