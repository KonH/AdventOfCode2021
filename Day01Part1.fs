module AdventOfCode2021.Day01Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 01 Part1"
    let measurements = File.ReadAllLines "Day01Part1.txt" |> Array.map Int32.Parse
    Console.WriteLine $"Found {measurements.Length} measurements"
    let changes = measurements |> Array.mapi (fun i current ->
        let next =
            match measurements |> Array.tryItem (i + 1) with
            | Some m -> m
            | None -> 0
        current, next)
    let largerMeasurements = changes |> Array.filter (fun pair ->
        let current, next = pair
        next > current)
    Console.WriteLine $"Found {largerMeasurements.Length} larger measurements"
    Console.WriteLine ""