module AdventOfCode2021.Day03Part2

open System
open System.IO

let run() =
    Console.WriteLine "Day 03 Part2"
    let inputs =
        File.ReadAllLines "Day03Part1.txt"
    Console.WriteLine $"Found {inputs.Length} inputs"
    let findTargetValue most =
        let length = inputs.[0].Length
        let mutable currentInputs = Array.copy inputs
        for bitPosition in 0..(length - 1) do
            if (currentInputs.Length > 1) then
                let bitsAtPosition = currentInputs |> Array.fold (fun state input -> if input.[bitPosition] = '1' then state + 1 else state) 0
                let condition = bitsAtPosition >= (currentInputs.Length - bitsAtPosition)
                let commonBit = if condition = most then '1' else '0'
                let newInputs = currentInputs |> Array.filter (fun input ->
                    input.[bitPosition] = commonBit)
                currentInputs <- newInputs
             else
                 ()
        let targetValue = currentInputs.[0]
        Convert.ToInt32(targetValue, 2)
    let oxygenGeneratorRating = findTargetValue true
    Console.WriteLine $"oxygenGeneratorRating is {oxygenGeneratorRating}"
    let CO2ScrubberRating = findTargetValue false
    Console.WriteLine $"CO2ScrubberRating is {CO2ScrubberRating}"
    Console.WriteLine $"Result is {oxygenGeneratorRating * CO2ScrubberRating}"
    Console.WriteLine ""