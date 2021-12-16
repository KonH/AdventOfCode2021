module AdventOfCode2021.Day15Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 15 Part1"
    let inputs =
        File.ReadAllLines "Day15Part1.txt"
    let state = inputs |> Array.map (fun line ->
        let charArray = line.ToCharArray()
        charArray |> Array.map (fun c -> Int32.Parse(c.ToString())))

    let getAt (x, y) st =
        let line = st |> Array.item y
        let it = line |> Array.item x
        it

    let tryGetAt (x, y) st =
        match st |> Array.tryItem y with
        | Some line ->
            match line |> Array.tryItem x with
            | Some it -> Some ((x, y), it)
            | _ -> None
        | _ -> None

    let neighborPositions (x, y) =
        [
                        (x, y + 1);
            (x - 1, y);             (x + 1, y);
                        (x, y - 1);
        ]

    let getNeighbors p =
        let potentialNeighbors =
            neighborPositions p |> List.map (fun p -> state |> tryGetAt p)
        let neighbors =
            potentialNeighbors
            |> List.filter (fun opt -> opt.IsSome)
            |> List.map (fun opt -> opt.Value)
            |> List.map fst
        neighbors

    let writePath (path: (int * int) list) =
        for y in [0..((state |> Array.length) - 1)] do
            for x in [0..((state[y] |> Array.length) - 1)] do
                let value = state[y][x]
                let prevColor = Console.ForegroundColor
                let inPath = path |> Seq.contains (x, y)
                Console.ForegroundColor <- if inPath then ConsoleColor.Green else prevColor
                Console.Write $" {value} "
                Console.ForegroundColor <- prevColor
            Console.WriteLine()
        Console.WriteLine()

    let start = (0, 0)
    let goal = (((state |> Array.length) - 1), ((state[0] |> Array.length) - 1))

    let fitPathDanger path =
        path
        |> List.skip 1
        |> List.map (fun p -> state |> getAt p)
        |> List.fold (+) 0

    let fitPathProximity (path: (int * int) list) =
        let x, y = path |> List.last
        let gx, gy = goal
        sqrt ((float gx - float x)**2. + (float gy - float y)**2.)

    let totalFit path =
        (1. / float (fitPathDanger path)) * (fitPathProximity path * 16.)

    let rec getNextPathsRec path depth =
        if depth = 0
        then
            let r1 = [path]
            r1
        else
            let mutable result = []
            getNeighbors (List.last path)
                |> List.map (fun next ->
                    let nextPath = List.append path [next]
                    let res = getNextPathsRec nextPath (depth - 1)
                    result <- res :: result
                    ()) |> ignore
            let r2 = result |> List.concat
            r2


    let getNextPaths path =
        let result = getNextPathsRec path 6
        result

    let simulate paths =
        let nextPaths = paths |> List.map getNextPaths |> List.concat
        Console.WriteLine $"Potential paths ({List.length nextPaths})"
        nextPaths
        |> List.map (fun p ->
            let fit = totalFit p
            let danger = fitPathDanger p
            (p, fit, danger))

    let rec simulateAll paths =
        let isSolved = paths |> List.exists (fun p -> List.last p = goal)
        if isSolved then
            paths |> List.map (fun p -> Console.WriteLine $"Result: {fitPathDanger p}") |> ignore
            ()
        else
            let nextPathFits = simulate paths
            let nextPaths =
                nextPathFits
                |> List.sortBy (fun (_, fit, danger) -> fit, danger)
                |> List.take 30
                |> List.map (fun (p, _, _) -> p)
            simulateAll nextPaths
            ()

    simulateAll [[start]]
