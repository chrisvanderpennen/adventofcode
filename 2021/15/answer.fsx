#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#r "nuget:Microsoft.Toolkit.HighPerformance"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes
open Microsoft.Toolkit.HighPerformance
open System.Collections.Generic

let inline dequeueUntilUnvisited (visited: HashSet<struct(int * int)>, queue: PriorityQueue<struct(int*int*int), int>) =
    let mutable found = struct(0,0,0)
    while (
        let _, entry, _ = queue.TryDequeue()
        let struct(a,b,_) = entry
        found <- entry
        visited.Contains(struct(a,b))
    ) do ()

    found

let inline dequeueUntilUnvisitedDebug (visited: HashSet<(int * int)>, queue: PriorityQueue<((int*int) list*int), int>) =
    let mutable found = ([],0)
    while (
        let _, entry, _ = queue.TryDequeue()
        let (pos::tail,_) = entry
        found <- entry
        visited.Contains(pos)
    ) do ()

    found

let inline wrapCost i = ((i - 1) % 9) + 1

let inline getCost (grid: Span2D<byte>, row, col) =
    let tileRow = row / grid.Height
    let tileCol = col / grid.Width
    let localRow = row % grid.Height
    let localCol = col % grid.Width
    let costOffset = tileRow + tileCol
    wrapCost (int(grid[localRow, localCol] &&& 0b0000_1111uy) + costOffset)

let inline visit(grid: Span2D<byte>, tileX, tileY, row, col, newCost, visited: HashSet<struct(int * int)>, queue: PriorityQueue<struct(int*int*int), int>) =
    if row < 0 || row >= grid.Height * tileX || col < 0 || col >= grid.Width * tileY then
        ()
    else
        let h = (grid.Width * tileX - col) + (grid.Height * tileY - row)
        if not (visited.Contains(struct(row, col))) then
            queue.Enqueue(struct(row, col, newCost), newCost + getCost(grid, row, col) + h)

let inline visitDebug(grid: Span2D<byte>, path, tileX, tileY, row, col, newCost, visited: HashSet<(int * int)>, queue: PriorityQueue<((int*int) list*int), int>, prevTable: Dictionary<int*int, int*int>, isBackwards) =
    if row < 0 || row >= grid.Height * tileX || col < 0 || col >= grid.Width * tileY then
        ()
    else
        let h = (grid.Width * tileX - col) + (grid.Height * tileY - row)
        if not (visited.Contains((row, col))) then
            // if isBackwards then
            //     prevTable[List.head path] <- (row, col)
            // else
            //     prevTable[(row, col)] <- List.head path
            queue.Enqueue(((row, col)::path, newCost), newCost + getCost(grid, row, col) + h)

let step(grid: Span2D<byte>, tileX, tileY, visited: HashSet<struct(int * int)>, queue: PriorityQueue<struct(int*int*int), int>) =
    
    let struct(row, col, cost) = dequeueUntilUnvisited(visited, queue)
    let newCost = cost + int (grid[row % grid.Height, col % grid.Width] &&& 0b0000_1111uy)
    ignore <| visited.Add(struct(row, col))

    visit(grid, tileX, tileY, row - 1, col, newCost, visited, queue)
    visit(grid, tileX, tileY, row + 1, col, newCost, visited, queue)
    visit(grid, tileX, tileY, row, col - 1, newCost, visited, queue)
    visit(grid, tileX, tileY, row, col + 1, newCost, visited, queue)

    struct(row, col, newCost)

let stepDebug(grid: Span2D<byte>, tileX, tileY, visited: HashSet<(int * int)>, queue: PriorityQueue<((int*int) list*int), int>, prevTable, isBackwards) =
    
    let (path, cost) = dequeueUntilUnvisitedDebug(visited, queue)
    let (row, col)::_ = path
    let newCost = cost + int (grid[row % grid.Height, col % grid.Width] &&& 0b0000_1111uy)
    ignore <| visited.Add((row, col))

    visitDebug(grid, path, tileX, tileY, row - 1, col, newCost, visited, queue, prevTable, isBackwards)
    visitDebug(grid, path, tileX, tileY, row + 1, col, newCost, visited, queue, prevTable, isBackwards)
    visitDebug(grid, path, tileX, tileY, row, col - 1, newCost, visited, queue, prevTable, isBackwards)
    visitDebug(grid, path, tileX, tileY, row, col + 1, newCost, visited, queue, prevTable, isBackwards)

    path, newCost

let inline dequeueUntilUnvisited2 (visited: HashSet<struct(int * int)>) (queue: PriorityQueue<struct(int*int), int>) =
    let rec inner () =
        match queue.TryDequeue() with
        | false, _, _ -> failwith "empty queue"
        | true, entry, _ when not(visited.Contains entry) -> entry
        | _ -> inner()
    inner()

let inline visit2 backwards tileX tileY current next (distances: Dictionary<struct(int*int), int>) (predecessors: Dictionary<struct(int*int), struct(int*int)>) (visited: HashSet<struct(int * int)>) (queue: PriorityQueue<struct(int*int), int>) (grid: Span2D<byte>) =
    let struct(row, col) = next
    if row < 0 || row >= grid.Height * tileX || col < 0 || col >= grid.Width * tileY then
        ()
    else
        let h = (grid.Width * tileX - col) + (grid.Height * tileY - row)
        let distance = distances[current] + getCost(grid, row, col)
        if not (visited.Contains(struct(row, col))) then
            match distances.TryGetValue next with
            | false, _ ->
                distances[next] <- distance
                if backwards then
                    predecessors[current] <- next
                else
                    predecessors[next] <- current
                queue.Enqueue(struct(row, col), distance + h)
            | true, distance' when distance' > distance ->
                distances[next] <- distance
                if backwards then
                    predecessors[current] <- next
                else
                    predecessors[next] <- current
                queue.Enqueue(struct(row, col), distance + h)
            | _ -> ()

let step2 backwards tileX tileY distances visited queue predecessors (grid: Span2D<byte>) =
    let struct(row, col) = dequeueUntilUnvisited2 visited queue
    visited.Add(struct(row, col))

    visit2 backwards tileX tileY struct(row, col) struct(row - 1, col) distances predecessors visited queue grid
    visit2 backwards tileX tileY struct(row, col) struct(row + 1, col) distances predecessors visited queue grid
    visit2 backwards tileX tileY struct(row, col) struct(row, col - 1) distances predecessors visited queue grid
    visit2 backwards tileX tileY struct(row, col) struct(row, col + 1) distances predecessors visited queue grid

    struct(row, col)

let partOne(input: Span<byte>) =
    let cols = input.IndexOf(byte '\n')
    let rows = (input.Length + 1) / (cols + 1)
    let grid = Span2D<byte>.DangerousCreate(&input.DangerousGetReference(), rows, cols, 1)

    let queue = PriorityQueue()
    let visited = HashSet()

    let mutable row = 0
    let mutable col = 0
    let mutable cost = 0
    let goalRow = rows - 1
    let goalCol = cols - 1

    // queue.Enqueue(([(0,1);(0,0)], 0), 0)
    // queue.Enqueue(([(1,0);(0,0)], 0), 0)
    // visited.Add((0,0)) |> ignore
    // let mutable path = []
    // while row <> goalRow || col <> goalCol do
    //     let next = stepDebug(grid, 1, 1, visited, queue)
    //     let ((nextRow, nextCol)::_, nextCost) = next
    //     row <- nextRow
    //     col <- nextCol
    //     cost <- nextCost
    //     path <- fst next

    // let pathSet = Set(path)

    // for printrow = 0 to grid.Height - 1 do
    //     for printcol = 0 to grid.Width - 1 do
    //         if pathSet.Contains((printrow, printcol)) then
    //             Console.ForegroundColor <- ConsoleColor.Red
    //         else if visited.Contains((printrow, printcol)) then
    //             Console.ForegroundColor <- ConsoleColor.Cyan
    //         else
    //             Console.ForegroundColor <- ConsoleColor.Gray
    //         printf "%i" (getCost (grid, printrow, printcol))
    //     printfn ""

    queue.Enqueue(struct(1,0,0), 0)
    queue.Enqueue(struct(0,1,0), 0)
    visited.Add(struct(0,0)) |> ignore
    while row <> goalRow || col <> goalCol do
        let struct(nextRow, nextCol, nextCost) =
            step(grid, 1, 1, visited, queue)
        row <- nextRow
        col <- nextCol
        cost <- nextCost

    cost

let partOneBidi(input: Span<byte>) =
    let cols = input.IndexOf(byte '\n')
    let rows = (input.Length + 1) / (cols + 1)
    let grid = Span2D<byte>.DangerousCreate(&input.DangerousGetReference(), rows, cols, 1)

    let queue = PriorityQueue()
    let visited = HashSet()
    let distances = Dictionary()
    distances[struct(0,0)] <- 0

    let revQueue = PriorityQueue()
    let revVisited = HashSet()
    let revDistances = Dictionary()
    revDistances[struct(rows - 1, cols - 1)] <- 0

    let prevTable = Dictionary()
    let rprevTable = Dictionary()

    queue.Enqueue(struct(0,0), 0)

    revQueue.Enqueue(struct(rows - 1, cols - 1), 0)

    let mutable next = struct(0,0)
    let mutable revNext = struct(0,0)

    while (
        next <- step2 false 1 1 distances visited queue prevTable grid
        revNext <- step2 false 1 1 revDistances revVisited revQueue rprevTable grid
        not(visited.Contains(revNext) || revVisited.Contains(next))
    ) do ()

    let rec makePath (tbl: Dictionary<_,_>) acc pos =
        match tbl.TryGetValue pos with
        | true, prev -> makePath tbl (prev::acc) prev
        | false, _ -> acc

    let intersection = if visited.Contains revNext then revNext else next

    let pathSet = 
        Set (makePath rprevTable [] intersection)
        |> Set.union (Set (makePath prevTable [] intersection))
        |> Set.add intersection
        |> Set.remove (0,0)

#if DEBUG || INTERACTIVE

    for printrow = 0 to grid.Height - 1 do
        for printcol = 0 to grid.Width - 1 do
            if pathSet.Contains(struct(printrow, printcol)) then
                Console.ForegroundColor <- ConsoleColor.Red
            else if visited.Contains(struct(printrow, printcol)) then
                if revVisited.Contains(struct(printrow, printcol)) then
                    Console.ForegroundColor <- ConsoleColor.DarkMagenta
                else
                    Console.ForegroundColor <- ConsoleColor.Cyan
            else if revVisited.Contains(struct(printrow, printcol)) then
                Console.ForegroundColor <- ConsoleColor.Green
            else
                Console.ForegroundColor <- ConsoleColor.Gray
            printf "%i" (getCost (grid, printrow, printcol))
        printfn ""

#endif

    let mutable cost = 0
    for struct(r,c) in pathSet do
        cost <- cost + getCost(grid, r, c)

    cost

let partTwo(input: Span<byte>) =
    let cols = input.IndexOf(byte '\n')
    let rows = (input.Length + 1) / (cols + 1)
    let grid = Span2D<byte>.DangerousCreate(&input.DangerousGetReference(), rows, cols, 1)

    let queue = PriorityQueue()
    let visited = HashSet()

    let mutable row = 0
    let mutable col = 0
    let mutable cost = 0
    let goalRow = rows * 5 - 1
    let goalCol = cols * 5 - 1

    // queue.Enqueue(([(0,1);(0,0)], 0), 0)
    // queue.Enqueue(([(1,0);(0,0)], 0), 0)
    // visited.Add((0,0)) |> ignore
    // let mutable path = []
    // while row <> goalRow || col <> goalCol do
    //     let next = stepDebug(grid, 5, 5, visited, queue)
    //     let ((nextRow, nextCol)::_, nextCost) = next
    //     row <- nextRow
    //     col <- nextCol
    //     cost <- nextCost
    //     path <- fst next

    // let pathSet = Set(path)

    // for printrow = 0 to grid.Height * 5 - 1 do
    //     for printcol = 0 to grid.Width * 5 - 1 do
    //         if pathSet.Contains((printrow, printcol)) then
    //             Console.ForegroundColor <- ConsoleColor.Red
    //         else
    //             Console.ForegroundColor <- ConsoleColor.Gray
    //         printf "%i" (getCost (grid, printrow, printcol))
    //     printfn ""

    queue.Enqueue(struct(1,0,0), 0)
    queue.Enqueue(struct(0,1,0), 0)
    visited.Add(struct(0,0)) |> ignore
    while row <> goalRow || col <> goalCol do
        let struct(nextRow, nextCol, nextCost) =
            step(grid, 5, 5, visited, queue)
        row <- nextRow
        col <- nextCol
        cost <- nextCost

    cost

let partTwoBidi(input: Span<byte>) =
    let cols = input.IndexOf(byte '\n')
    let rows = (input.Length + 1) / (cols + 1)
    let grid = Span2D<byte>.DangerousCreate(&input.DangerousGetReference(), rows, cols, 1)

    let queue = PriorityQueue()
    let visited = HashSet()
    let distances = Dictionary()
    distances[struct(0,0)] <- 0

    let revQueue = PriorityQueue()
    let revVisited = HashSet()
    let revDistances = Dictionary()
    revDistances[struct(rows * 5 - 1, cols * 5 - 1)] <- 0

    let prevTable = Dictionary()
    let rprevTable = Dictionary()

    queue.Enqueue(struct(0,0), 0)

    revQueue.Enqueue(struct(rows * 5 - 1, cols * 5 - 1), 0)

    let mutable next = struct(0,0)
    let mutable revNext = struct(0,0)

    while (
        next <- step2 false 5 5 distances visited queue prevTable grid
        revNext <- step2 false 5 5 revDistances revVisited revQueue rprevTable grid
        not(visited.Contains(revNext) || revVisited.Contains(next))
    ) do ()

    let rec makePath (tbl: Dictionary<_,_>) acc pos =
        match tbl.TryGetValue pos with
        | true, prev -> makePath tbl (prev::acc) prev
        | false, _ -> acc

    let intersection = if visited.Contains revNext then revNext else next

    let pathSet = 
        Set (makePath rprevTable [] intersection)
        |> Set.union (Set (makePath prevTable [] intersection))
        |> Set.add intersection
        |> Set.remove (0,0)

#if DEBUG || INTERACTIVE

    for printrow = 0 to grid.Height * 5 - 1 do
        for printcol = 0 to grid.Width * 5 - 1 do
            if pathSet.Contains(struct(printrow, printcol)) then
                Console.ForegroundColor <- ConsoleColor.Red
            else if visited.Contains(struct(printrow, printcol)) then
                if revVisited.Contains(struct(printrow, printcol)) then
                    Console.ForegroundColor <- ConsoleColor.DarkMagenta
                else
                    Console.ForegroundColor <- ConsoleColor.Cyan
            else if revVisited.Contains(struct(printrow, printcol)) then
                Console.ForegroundColor <- ConsoleColor.Green
            else
                Console.ForegroundColor <- ConsoleColor.Gray
            printf "%i" (getCost (grid, printrow, printcol))
        printfn ""

#endif

    let mutable cost = 0
    for struct(r,c) in pathSet do
        cost <- cost + getCost(grid, r, c)

    cost

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadBinary ())

    [<Benchmark>]
    member public this.PartOneBidi () =
        partOneBidi(this.loadBinary ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadBinary ())

    [<Benchmark>]
    member public this.PartTwoBidi () =
        partTwoBidi( this.loadBinary ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartOneBidi ())
printfn "%A" (answer.PartTwo ())
printfn "%A" (answer.PartTwoBidi ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
