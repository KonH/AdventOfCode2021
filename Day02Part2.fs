module AdventOfCode2021.Day02Part2

open System
open System.IO

type State = {
    Horizontal: int32
    Depth: int32
    Aim: int32
}

let run() =
    Console.WriteLine "Day 02 Part2"
    let inputs =
        File.ReadAllLines "Day02Part1.txt"
        |> Array.map (fun line ->
            let lineParts = line.Split(' ')
            let command = lineParts.[0]
            let value = Int32.Parse lineParts.[1]
            (command, value))
    Console.WriteLine $"Found {inputs.Length} inputs"
    let update state cmd =
        let choose, value = cmd
        match choose with
        | "down" -> { state with Aim = state.Aim + value }
        | "up" -> { state with Aim = state.Aim - value }
        | "forward" -> { state with Horizontal = state.Horizontal + value; Depth = state.Depth + state.Aim * value }
        | _ -> state
    let state = {
        Horizontal = 0
        Depth = 0
        Aim = 0
    }
    let finalState = inputs |> Array.fold update state
    let x, y = finalState.Horizontal, finalState.Depth
    Console.WriteLine $"Final position is ({x}, {y})"
    Console.WriteLine $"Result is {x * y}"
    Console.WriteLine ""