module AdventOfCode2021.Day06Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 06 Part1"
    let inputs =
        File.ReadAllLines "Day06Part1.txt"
    let initialState = inputs.[0].Split ',' |> Array.map Int32.Parse |> List.ofArray
    let updateTimer timer =
        if timer > 0 then timer - 1 else 6
    let tryProduce timer =
        if timer = 0 then [8] else []
    let rec updateState oldState newState =
        match oldState with
        | head :: tail -> updateState tail (updateTimer head :: newState @ tryProduce head)
        | [] -> newState
    let update (state: int list) day =
        let newState = updateState state []
        Console.WriteLine $"Day #{day}: {List.length newState}"
        newState
    let result = [1..80] |> Seq.fold update initialState
    Console.WriteLine $"Result is {List.length result}"