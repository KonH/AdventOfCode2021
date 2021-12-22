// https://github.com/bjorkstromm/AdventOfCode2021/blob/main/day17.fsx
module AdventOfCode2021.Day17Part2

open System

let next ((x, y), (vx, vy)) =
    let x', y' = (x + vx, y + vy)
    let vx', vy' = (vx - sign vx, vy - 1)
    (x', y'), (vx', vy')

let isHit xRange yRange initVel =
    let inRange (min, max) v =
        v >= min && v <= max

    let _, maxX = xRange
    let minY, _ = yRange

    let rec loop pos vel maxH =
        match next (pos, vel) with
        | (x,y), _ when x > maxX || y < minY -> None
        | (x,y), _ when x |> inRange xRange && y |> inRange yRange -> Some (initVel, maxH)
        | (x,y), vel' -> loop (x,y) vel' (max maxH y)

    loop (0,0) initVel 0

let getHits xRange yRange =
    let _, maxX = xRange
    let minY, _ = yRange
    [1..maxX]
    |> List.map (fun x ->
        [minY..abs minY]
        |> List.map (fun y -> (x,y)))
    |> List.concat
    |> List.choose (isHit xRange yRange)

let getInput filename =
    let parseRange (str : string) =
        match str.Substring(2).Split("..") with
        | [|min;max|] -> (min |> Convert.ToInt32, max |> Convert.ToInt32)
        | _ -> failwithf $"Invalid range %s{str}"

    let parseRanges x y =
        (parseRange x, parseRange y)

    match (filename |> System.IO.File.ReadAllText).Split(": ") with
    | [|_;ranges|] ->
        match ranges.Split(", ") with
        | [|x; y|] when x.StartsWith "x=" &&
                        y.StartsWith "y=" -> parseRanges x y
        | [|y; x|] when x.StartsWith "x=" &&
                        y.StartsWith "y=" -> parseRanges x y
        | _ -> failwithf "Invalid input"
    | _ -> failwithf "Invalid input"

let run() =
    Console.WriteLine "Day 17 Part2"
    let result =
        "Day17Part1.txt"
        |> getInput
        ||> getHits
        |> List.distinctBy fst
        |> List.length
    Console.WriteLine result