module AdventOfCode2021.Day13Part1

open System
open System.IO

type FoldType = Vertical | Horizontal

let run() =
    Console.WriteLine "Day 13 Part1"
    let inputs =
        File.ReadAllLines "Day13Part1.txt"

    let isEmpty s =
        String.IsNullOrEmpty s

    let isNotEmpty s =
        isEmpty s = false

    let mutable points =
        inputs
        |> Array.takeWhile isNotEmpty
        |> Array.map (fun l ->
            let parts = l.Split ','
            (Int32.Parse parts[0], Int32.Parse parts[1]))

    let folds =
        inputs
        |> Array.skipWhile isNotEmpty
        |> Array.skip 1
        |> Array.map (fun l ->
            let fold = l.Substring("fold along ".Length)
            let parts = fold.Split '='
            ((if parts[0] = "x" then Vertical else Horizontal), Int32.Parse parts[1]))

    let writeState ps =
        let points =
            ps |> Array.length
        Console.WriteLine $"Points: {points}"
        Console.WriteLine ""

    let foldVal line v =
        Math.Abs (v - line * 2)

    let foldVertical line (x, y) =
        if x > line then (foldVal line x, y) else (x, y)

    let foldHorizontal line (x, y) =
        if y > line then (x, foldVal line y) else (x, y)

    let applyFold (axis, line) p =
        match axis with
        | Vertical -> p |> foldVertical line
        | Horizontal -> p |> foldHorizontal line

    let makeFold ps fold =
        ps
        |> Array.map (applyFold fold)
        |> Array.distinct

    Console.WriteLine "Initial:"
    points |> writeState

    let fold = folds[0]
    Console.WriteLine $"Fold: {fold}"
    points <- (points, fold) ||> makeFold
    points |> writeState

    Console.WriteLine $"Result is {Array.length points}"