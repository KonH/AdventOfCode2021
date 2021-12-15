module AdventOfCode2021.Day13Part2

open System
open System.IO

type FoldType = Vertical | Horizontal

let run() =
    Console.WriteLine "Day 13 Part2"
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

    let getAt pos ps =
        ps |> Array.tryFind (fun p -> p = pos)

    let getCharAt pos ps =
       let point = ps |> getAt pos
       match point with
       | Some _ -> '#'
       | None -> '.'

    let writeState ps =
        let height =
            ps
            |> Array.map snd
            |> Array.max
        let width =
            ps
            |> Array.map fst
            |> Array.max
        for y in [0..height] do
            for x in [0..width] do
                let ch = ps |> getCharAt (x, y)
                Console.Write ch
            Console.WriteLine ""
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

    for fold in folds do
        Console.WriteLine $"Fold: {fold}"
        points <- (points, fold) ||> makeFold

    points |> writeState