module AdventOfCode2021.Day06Part2

open System
open System.Diagnostics
open System.IO

let run() =
    let sw = Stopwatch()
    sw.Start()
    Console.WriteLine "Day 06 Part2"
    let inputs =
        File.ReadAllLines "Day06Part1.txt"
    let initialState = inputs.[0].Split ',' |> Array.map Int32.Parse |> List.ofArray
    let initialArray = Array.create 9 0L
    for item in initialState do
        initialArray.[item] <- initialArray.[item] + int64(1)
    let mutable state = (initialArray, int64(List.length initialState), 0L)
    let updateState() =
        let array, count, prevNewCount = state
        let newCount = array.[0]
        for i in [0..7] do
            array.[i] <- array.[i + 1]
        array.[5] <- array.[5] + prevNewCount
        array.[8] <- newCount
        state <- (array, count + prevNewCount, newCount)
        ()
    let update day =
        updateState()
        let _, count, _ = state
        Console.WriteLine $"Day #{day}: {count}"
        ()
    for day in [1..256] do
        update day
    let _, count, newCount = state
    Console.WriteLine $"Result is {count + newCount}"
    sw.Stop()
    Console.WriteLine $"Execution time: {sw.Elapsed.TotalSeconds} seconds"