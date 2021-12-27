module AdventOfCode2021.Day25Part1

open System
open System.IO

type Cell =
    | Down
    | Right

let getInput path =
    let lines = File.ReadAllLines path
    let width = lines[0].Length
    let height = lines.Length
    let map =
        lines
        |> Seq.mapi (fun y line ->
            line
            |> Seq.mapi (fun x c ->
                let cell =
                    match c with
                    | 'v' -> Some Down
                    | '>' -> Some Right
                    | _ -> None
                ((x, y), cell)))
        |> Seq.concat
        |> Map.ofSeq
        |> Map.filter (fun _ x -> x.IsSome)
        |> Map.map (fun _ x -> x.Value)
    (width, height, map)

let getRight (x, y) (w, _) =
    let newX = x + 1
    if newX = w
    then (0, y)
    else (newX, y)

let getDown (x, y) (_, h) =
    let newY = y + 1
    if newY = h
    then (x, 0)
    else (x, newY)

let simulate state =
    let mutable isMoved = false
    let w, h, map = state
    let firstMap =
        map |> Map.fold (fun s k v ->
            match v with
            | Right ->
                let rightPos = (w, h) |> getRight k
                let right = map |> Map.tryFind rightPos
                let newKey =
                    match right with
                    | Some _ -> k
                    | None ->
                        isMoved <- true
                        rightPos
                s |> Map.add newKey v
            | Down -> s |> Map.add k v
        ) Map.empty
    let secondMap =
        firstMap |> Map.fold (fun s k v ->
            match v with
                | Right -> s |> Map.add k v
                | Down ->
                    let downPos = (w, h) |> getDown k
                    let down = firstMap |> Map.tryFind downPos
                    let newKey =
                        match down with
                        | Some _ -> k
                        | None ->
                            isMoved <- true
                            downPos
                    s |> Map.add newKey v
        ) Map.empty
    ((w, h, secondMap), isMoved)

let write (w, h, map) =
    for y in 0..(h - 1) do
        for x in 0..(w - 1) do
            let cell = map |> Map.tryFind (x, y)
            let c =
                match cell with
                | Some Down -> 'v'
                | Some Right -> '>'
                | None -> '.'
            Console.Write c
        Console.WriteLine ""
    Console.WriteLine ""
    ()

let rec simulateLoop state step =
    Console.WriteLine $"Step {step}:"
    let newState, isMoved = state |> simulate
    newState |> write
    if isMoved then simulateLoop newState (step + 1) else ()

let run() =
    Console.WriteLine "Day 25 Part1"
    let initial =
        "Day25Part1.txt"
        |> getInput
    Console.WriteLine "Initial:"
    write initial
    simulateLoop initial 1
    Console.WriteLine ""