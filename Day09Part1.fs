module AdventOfCode2021.Day09Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 09 Part1"
    let inputs =
        File.ReadAllLines "Day09Part1.txt"
    let field = inputs |> Array.map (fun input -> input.ToCharArray() |> Array.map (fun c -> Int32.Parse(c.ToString())))
    let getPoint x y =
        match field |> Array.tryItem y with
        | Some line -> line |> Array.tryItem x
        | _ -> None
    let calcAdjacentPoints x y =
        let point = field.[y].[x]
        let points = [(x, (y - 1)); (x, (y + 1)); ((x - 1), y); ((x + 1), y)]
        let pointValues = points |> List.map (fun p ->
            let x, y = p
            getPoint x y)
        let actualValues = pointValues |> List.filter (fun p -> p.IsSome) |> List.map (fun p -> p.Value)
        let isLowPoint = actualValues |> List.forall (fun x -> point < x)
        (point, isLowPoint)
    let adjacentPoints = field |> Array.mapi (fun y line -> line |> Array.mapi (fun x _ -> calcAdjacentPoints x y))
    let lowPoints = adjacentPoints |> Array.map (fun line -> line |> Array.filter (fun p ->
            let _, isLowPoint = p
            isLowPoint)) |> Array.filter (fun line -> Array.isEmpty line = false)
    let riskLevelSum = lowPoints |> Array.sumBy (fun line -> line |> Array.sumBy (fun p ->
            let height, _ = p
            height + 1))
    let result = riskLevelSum
    Console.WriteLine $"Result is {result}"