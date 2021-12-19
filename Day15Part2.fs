// https://github.com/bjorkstromm/AdventOfCode2021/blob/main/day15.fsx
module AdventOfCode2021.Day15Part2

open System
open System.Collections.Generic

let getMap filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Seq.mapi (fun y line ->
        line
        |> Seq.mapi (fun x c ->
            let v = c |> Char.GetNumericValue |> int
            ((x,y),v)))
    |> Seq.concat
    |> Map.ofSeq

let getAdjacent (x,y) =
    seq {
        (x - 1, y)
        (x + 1, y)
        (x, y + 1)
        (x, y - 1)
    }

let findPaths (map : Map<int * int, int>) =
    let start = (0,0)
    let end' = (map.Keys |> Seq.maxBy fst |> fst, map.Keys |> Seq.maxBy snd |> snd)

    let queue = PriorityQueue<int * int, int>()
    let visited = Dictionary<int * int, int>()

    visited.[start] <- 0
    queue.Enqueue(start, 0)

    let rec loop () =
        if queue.Count = 0 then ()
        else
            let c = queue.Dequeue()
            c
            |> getAdjacent
            |> Seq.filter (fun a -> map.ContainsKey(a) && not(visited.ContainsKey(a)))
            |> Seq.iter (fun a ->
                visited.[a] <- visited.[c] + map.[a]
                if a <> end' then
                    queue.Enqueue(a, visited.[a]))
            loop()

    loop()
    visited.[end']

let scaleMap (scale : int) (map : Map<int * int, int>) =
    let xMax, yMax = ((map.Keys |> Seq.maxBy fst |> fst) + 1, (map.Keys |> Seq.maxBy snd |> snd) + 1)

    [0..(yMax * scale) - 1]
    |> Seq.map (fun y ->
        [0..(xMax * scale) - 1]
        |> Seq.map (fun x ->
            let x' = x % xMax
            let y' = y % yMax
            let distance = (y / yMax) + (x / xMax)
            let risk = ((map.[(x',y')] + distance - 1) % 9) + 1
            ((x,y), risk)
            )
        )
    |> Seq.concat
    |> Map.ofSeq

let run() =
    Console.WriteLine "Day 15 Part2"
    let result =
        "Day15Part1.txt"
        |> getMap
        |> scaleMap 5
        |> findPaths
    Console.WriteLine result
