module AdventOfCode2021.Day03Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 03 Part1"
    let inputs =
        File.ReadAllLines "Day03Part1.txt"
    Console.WriteLine $"Found {inputs.Length} inputs"
    let oneBitCounts = Array.create 12 0
    for input in inputs do
        for i in 0..(input.Length - 1) do
            (if input.[i] = '1' then Array.set oneBitCounts i (oneBitCounts.[i] + 1) else ()) |> ignore
    let halfRows = inputs.Length / 2
    let findCommonBits most =
        let bits =
            oneBitCounts
            |> Array.map (fun v ->
                let condition = if most then v > halfRows else v < halfRows
                if condition then '1' else '0')
        let str = new string(bits)
        Convert.ToInt32(str, 2)
    let gamma = findCommonBits true
    Console.WriteLine $"Gamma is {gamma}"
    let epsilonRate = findCommonBits false
    Console.WriteLine $"Epsilon rate is {epsilonRate}"
    Console.WriteLine $"Result is {gamma * epsilonRate}"
    Console.WriteLine ""