module AdventOfCode2021.Day07Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 07 Part1"
    let inputs =
        File.ReadAllLines "Day07Part1.txt"
    let positions = inputs.[0].Split(',') |> Array.map Int32.Parse
    let calculateCost position =
        positions |> Array.map (fun p -> Math.Abs(position - p)) |> Array.sum
    let costs = positions |> Array.map calculateCost
    let result = costs |> Array.min
    Console.WriteLine $"Result is {result}"