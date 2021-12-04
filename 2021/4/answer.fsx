#load "../util.fsx"
open System
open System.Collections.Generic

type Board = {
    numbers: HashSet<int>
    rows: HashSet<int>[]
    columns: HashSet<int>[]
}

let loadBoards () =
    let numbers_::_::boardInput = loadLines id
    let numbers = numbers_.Split ',' |> Array.map int

    let makeBoard () =
        {
            numbers = HashSet()
            rows = [|
                HashSet()
                HashSet()
                HashSet()
                HashSet()
                HashSet()
            |]
            columns = [|
                HashSet()
                HashSet()
                HashSet()
                HashSet()
                HashSet()
            |]
        }

    let (boards_, lastBoard, _) =
        boardInput
        |> List.map (fun s -> s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
        |> List.fold (
            fun (boards, currentBoard, i) ->
                function
                | [||] -> currentBoard::boards, makeBoard(), 0
                | row -> 
                    currentBoard.numbers.UnionWith row
                    currentBoard.rows[i].UnionWith row
                    Array.iteri (fun i v -> ignore <| currentBoard.columns[i].Add v) row
                    boards, currentBoard, i + 1
            ) (List.empty, makeBoard(), 0)

    let boards = lastBoard::boards_ |> List.rev
    numbers, boards

let testBoard number (board: Board) =
    if board.numbers.Remove number then
        if board.rows |> Array.exists (fun s -> s.Remove number && s.Count = 0)
        then true
        else board.columns |> Array.exists (fun s -> s.Remove number && s.Count = 0)
    else false

let part1 () =

    let numbers, boards = loadBoards ()

    let (Some (winner, number)) =
        numbers
        |> Array.tryPick (
            fun number ->
                boards |> List.tryFind (testBoard number) |> Option.map (fun board -> board, number)
            )

    printfn "%A %A %A" number ((Seq.reduce (+) winner.numbers) * number) winner

#time
part1 ()
#time

let part2 () =
    let numbers, boards = loadBoards ()
    
    let (Some number), winner::_ =
        numbers
        |> Array.fold
            (fun (lastWinningNumber, boards) number ->
                match lastWinningNumber with
                | Some _ -> lastWinningNumber, boards
                | None ->
                    match boards with
                    | [board] -> 
                        if testBoard number board then
                            Some number, boards
                        else
                            None, boards
                    | _ -> None, List.filter (testBoard number >> not) boards
            )
            (None, boards)

    printfn "%A %A %A" number ((Seq.reduce (+) winner.numbers) * number) winner

#time
part2()
#time
