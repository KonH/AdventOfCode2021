module AdventOfCode2021.Day09Part2

open System
open System.Collections.Generic
open System.IO

let run() =
    Console.WriteLine "Day 09 Part2"
    let inputs =
        File.ReadAllLines "Day09Part1.txt"
    let field = inputs |> Array.map (fun input -> input.ToCharArray() |> Array.map (fun c -> Int32.Parse(c.ToString())))
    let getPoint x y =
        match field |> Array.tryItem y with
        | Some line -> line |> Array.tryItem x
        | _ -> None
    let getPointWithCoords x y =
        match getPoint x y with
        | Some p -> Some ((x, y), p)
        | _ -> None
    let calcAdjacentPoints x y =
        let point = field.[y].[x]
        let points = [(x, (y - 1)); (x, (y + 1)); ((x - 1), y); ((x + 1), y)]
        let pointValues = points |> List.map (fun p ->
            let x, y = p
            getPointWithCoords x y)
        let actualValues = pointValues |> List.filter (fun p -> p.IsSome) |> List.map (fun p -> p.Value)
        let isLowPoint = actualValues |> List.forall (fun x ->
            let _, xValue = x
            point < xValue)
        ((x, y), point, isLowPoint, actualValues)
    let adjacentPoints = field |> Array.mapi (fun y line -> line |> Array.mapi (fun x _ -> calcAdjacentPoints x y))
    let rec fillBasinElements value adjPoints (result: Dictionary<Tuple<int, int>, int>) =
        let elements = adjPoints |> List.filter (fun adj ->
            let _, v = adj
            (v >= value) && (v < 9))
        elements |> List.map (fun adj ->
            let (x, y), v = adj
            let subAdj = adjacentPoints.[y].[x]
            let _, _, _, subAdjPoints = subAdj
            if result.TryAdd((x, y), v) then fillBasinElements v subAdjPoints result else ()) |> ignore
        ()
    let fillBasin ap (result: Dictionary<Tuple<int, int>, int>) =
        let _, point, _, adjPoints = ap
        fillBasinElements point adjPoints result
        result
    let createBasin ap =
        let (x, y), point, _, _ = ap
        let result = Dictionary<Tuple<int, int>, int>()
        result.Add((x, y), point)
        fillBasin ap result
    let tryCreateBasin ap =
        let _, _, isLowPoint, _ = ap
        if isLowPoint then createBasin ap else Dictionary<Tuple<int, int>, int>()
    let basins = adjacentPoints |> Array.map (fun line -> line |> Array.map tryCreateBasin) |> Array.concat
    let topThreeBySize = basins |> Array.sortByDescending (fun b -> b.Count) |> Array.take 3
    let result = topThreeBySize |> Array.fold (fun s v -> s * v.Count) 1
    Console.WriteLine $"Result is {result}"