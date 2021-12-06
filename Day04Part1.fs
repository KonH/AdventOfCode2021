module AdventOfCode2021.Day04Part1

open System
open System.IO

let run() =
    Console.WriteLine "Day 04 Part1"
    let inputs =
        File.ReadAllLines "Day04Part1.txt"
    Console.WriteLine $"Found {inputs.Length} inputs"
    let lookupBoards _ =
        let mutable boards = []
        let mutable currentBoard = []
        let boardLines = Seq.append (inputs |> Seq.skip 2) [""]
        for boardLine in boardLines do
            if boardLine = "" then
                boards <- (List.toArray currentBoard, false) :: boards
                currentBoard <- []
            else
                let line = (boardLine.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun v -> (Int32.Parse v, false)))
                currentBoard <- line :: currentBoard
        boards

    let isFinishCell cell =
        let _, mark = cell
        mark
    let hasFinishLine board =
        board |> Array.exists (fun line -> line |> Array.forall isFinishCell)
    let hasFinishColumn (board: ('a * bool) [] []) =
        let columns = [0..(Array.length board.[0] - 1)]
        columns
        |> List.map (fun column ->
            let columnCells = board |> Array.map (fun line -> line.[column])
            columnCells |> Array.forall isFinishCell)
        |> List.exists id
    let isWinnerBoard board =
        hasFinishLine board || hasFinishColumn board

    let updateCell number cell =
        let num, mark = cell
        let newMark = mark || (num = number)
        Console.Write (if newMark then $"[{num}] " else $"{num} ")
        (num, newMark)
    let updateLine number line =
        Console.WriteLine ""
        line |> Array.map (updateCell number)
    let updateBoard number boardState =
        let board, _ = boardState
        let newBoard = board |> Array.map (updateLine number)
        let newWinner = isWinnerBoard newBoard
        Console.WriteLine ""
        (newBoard, newWinner)
    let updateState state number =
        let boards, finish, _ = state
        if finish then
            state
        else
            Console.WriteLine ""
            Console.WriteLine $"Update ({number}):"
            let newBoards = boards |> List.map (updateBoard number)
            let newFinish = newBoards |> List.exists (fun boardState ->
                let _, winner = boardState
                winner)
            Console.WriteLine ""
            Console.WriteLine $"Is finished? {newFinish}"
            let finalNumber = if newFinish then number else 0
            (newBoards, newFinish, finalNumber)

    let calculateCellScore cell =
        let num, mark = cell
        if mark then 0 else num
    let calculateBoardScore finalNumber boardState =
        let board, winner = boardState
        if winner
        then
            let unmatchedCellSum = board |> Array.sumBy (fun line -> line |> Array.sumBy calculateCellScore)
            unmatchedCellSum * finalNumber
        else
            0

    let numbers = inputs.[0].Split(',') |> Array.map Int32.Parse
    let initialState = (lookupBoards (), false, 0)
    let finalBoards, _, finalNumber = numbers |> Array.fold updateState initialState
    let bestScore = finalBoards |> List.map (calculateBoardScore finalNumber) |> List.max
    Console.WriteLine $"Best score is {bestScore}"