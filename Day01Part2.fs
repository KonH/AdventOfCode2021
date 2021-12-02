module AdventOfCode2021.Day01Part2

open System
open System.IO

let run() =
    Console.WriteLine "Day 01 Part2"
    let measurements = File.ReadAllLines "Day01Part1.txt" |> Array.map Int32.Parse
    Console.WriteLine $"Found {measurements.Length} measurements"
    let windows =
        measurements
        |> Array.mapi (fun i first ->
            let second = measurements |> Array.tryItem (i + 1)
            let third = measurements |> Array.tryItem (i + 2)
            first, second, third)
        |> Array.filter (fun x ->
            let _, second, third = x
            second.IsSome && third.IsSome)
        |> Array.map (fun x ->
            let first, second, third = x
            first + second.Value + third.Value)
    let changes = windows |> Array.mapi (fun i current ->
        let next =
            match windows |> Array.tryItem (i + 1) with
            | Some m -> m
            | None -> 0
        current, next)
    let largerMeasurements = changes |> Array.filter (fun pair ->
        let current, next = pair
        next > current)
    Console.WriteLine $"Found {largerMeasurements.Length} larger measurements"
    Console.WriteLine ""