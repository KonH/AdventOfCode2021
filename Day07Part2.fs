module AdventOfCode2021.Day07Part2

open System
open System.IO

let run() =
    Console.WriteLine "Day 07 Part2"
    let inputs =
        File.ReadAllLines "Day07Part1.txt"
    let positions = inputs.[0].Split(',') |> Array.map Int32.Parse
    let calculateCostValue (oldPosition: int) (newPosition: int) =
       let length = Math.Abs(oldPosition - newPosition)
       let result =
           if length = 0
           then 0
           else [0..length] |> Seq.sum
       result
    let calculateCost position =
        positions |> Array.map (fun p -> calculateCostValue p position) |> Array.sum
    let possiblePositions = [positions |> Array.min..positions |> Array.max]
    let costs = possiblePositions |> Seq.map (fun p -> (p, calculateCost p))
    let result = costs |> Seq.minBy (fun p ->
        let _, cost = p
        cost)
    Console.WriteLine $"Result is {result}"