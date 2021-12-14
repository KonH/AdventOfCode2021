module AdventOfCode2021.Day11Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 11 Part1"
    let inputs =
        File.ReadAllLines "Day11Part1.txt"
    let state = inputs |> Array.map (fun line ->
        let charArray = line.ToCharArray()
        charArray |> Array.map (fun c -> Int32.Parse(c.ToString())))
    let flashState = state |> Array.map (fun line -> line |> Array.map(fun _ -> false))

    let iterate handle (st: _[][]) =
        for y in [0..((st |> Array.length) - 1)] do
            for x in [0..((st.[y] |> Array.length) - 1)] do
                let value = st.[y].[x]
                handle (x, y) value

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

    let updateAt (x, y) value (st: _[][]) =
        st.[y].[x] <- value

    let fold folder initial st =
        let mutable foldSt = initial
        st |> iterate (fun _ v -> foldSt <- folder foldSt v)
        foldSt

    let neighborPositions (x, y) =
        [
            (x - 1, y + 1); (x, y + 1); (x + 1, y + 1);
            (x - 1, y);                 (x + 1, y);
            (x - 1, y - 1); (x, y - 1); (x + 1, y - 1);
        ]

    let getNeighbors p st =
        let potentialNeighbors =
            neighborPositions p |> List.map (fun p -> st |> tryGetAt p)
        let neighbors =
            potentialNeighbors |> List.filter (fun opt -> opt.IsSome) |> List.map (fun opt -> opt.Value)
        neighbors

    let writeState (st: int32[][]) =
        for y in [0..((st |> Array.length) - 1)] do
            for x in [0..((st.[y] |> Array.length) - 1)] do
                let value = st.[y].[x]
                let prevColor = Console.ForegroundColor
                Console.ForegroundColor <- if value = 0 then ConsoleColor.Green else prevColor
                Console.Write $" {value} "
                Console.ForegroundColor <- prevColor
            Console.WriteLine()
        Console.WriteLine()

    let reset (st: bool[][]) =
        st |> iterate (fun p _ -> st |> updateAt p false)

    let increaseAll st =
        st |> iterate (fun p v -> st |> updateAt p (v + 1))

    let flash v =
        v > 9

    let rec tryFlash p v (stEnergy, stFlash: bool[][]) =
        let alreadyFlashed = stFlash |> getAt p
        if (not alreadyFlashed) && flash v
        then
            stFlash |> updateAt p true
            let neighbors = stEnergy |> getNeighbors p
            neighbors |> List.map ( fun n ->
                let np, prevNv = n
                let nv = prevNv + 1
                stEnergy |> updateAt np nv) |> ignore
        else ()

    let countFlashes st =
        st |> fold (fun st v -> if v then (st + 1) else st) 0

    let rec flashAll (stEnergy, stFlash) =
        let prevFlashes = stFlash |> countFlashes
        stEnergy |> iterate (fun p v -> (stEnergy, stFlash) |> tryFlash p v)
        let newFlashes = stFlash |> countFlashes
        if newFlashes > prevFlashes
        then (stEnergy, stFlash) |> flashAll
        else ()

    let handleFlashed stEnergy =
        stEnergy |> iterate (fun p v ->
            if flash v
            then
                stEnergy |> updateAt p 0
            else())

    let mutable totalFlashes = 0

    let simulate step =
        flashState |> reset
        state |> increaseAll
        (state, flashState) |> flashAll
        state |> handleFlashed
        Console.WriteLine $"Step #{step}"
        writeState state
        let flashesAtStep = flashState |> countFlashes
        Console.WriteLine $"Flashes: {flashesAtStep}"
        totalFlashes <- totalFlashes + flashesAtStep

    writeState state
    for step in [1..100] do
        simulate step

    Console.WriteLine $"Result is {totalFlashes}"