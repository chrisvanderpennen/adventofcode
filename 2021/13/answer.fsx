#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#r "nuget:Microsoft.Toolkit.HighPerformance"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes

open Microsoft.Toolkit.HighPerformance
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open Microsoft.FSharp.NativeInterop

[<Struct>]
type Axis =
| X
| Y

[<Struct>]
type Fold = Fold of axis: Axis * position: int

[<Struct>]
type Point = Point of x: int * y: int

let inline numbersToInt (numbers: Span<byte>) =
    let mutable res = 0
    for i = 0 to numbers.Length - 1 do
        res <- ((res <<< 1) + (res <<< 3)) + int(numbers[i] - (byte '0'))
    res

let parseInput (input: Span<byte>) =
    let mutable xMax = 0
    let mutable yMax = 0
    let pointCount = input.Count(byte ',')
    let foldCount = input.Count(byte '=')
    let points = Array.zeroCreate<Point> pointCount

    let mutable pointIndex = 0
    let mutable enumerator = input.Tokenize(byte '\n')
    enumerator.MoveNext () |> ignore
    
    // parse points
    while (
        let line = enumerator.Current
        let commaPos = line.IndexOf(byte ',')
        let x = numbersToInt(line.Slice(0, commaPos))
        let y = numbersToInt(line.Slice(commaPos + 1))
        points[pointIndex] <- Point(x,y)
        incr &pointIndex
        xMax <- max xMax x
        yMax <- max yMax y

        enumerator.MoveNext () && enumerator.Current.Length > 0
    ) do ()
    // parse folds

    let folds = Array.zeroCreate<Fold> foldCount
    let mutable foldIndex = 0
    while enumerator.MoveNext () do
        let line = enumerator.Current
        let eqPos = line.IndexOf (byte '=')
        let xy = if line.Slice(eqPos - 1, 1)[0] = byte 'x' then Y else X
        let col = numbersToInt(line.Slice (eqPos+1))
        folds[foldIndex] <- Fold (xy,col)
        incr &foldIndex

    struct(points, folds, xMax + 1, yMax + 1)

let inline foldX (grid: Span2D<byte>, row: int) =
    #if DEBUG
    printfn "fold y=%A" row
    for v in grid.GetRow row do
        if v <> 0uy then
            failwith "bad fold"
    #endif
    let top = grid.Slice(0, 0, row, grid.Width)
    let bottom = grid.Slice(row + 1, 0, grid.Height - (row + 1), grid.Width)
    let target = if top.Height >= bottom.Height then top else bottom
    let source = if top.Height >= bottom.Height then bottom else top
    for i = 0 to source.Height - 1 do
        let mutable sourceRow = source.GetRowSpan (i)
        let mutable targetRow = target.GetRowSpan (target.Height - 1 - i)
        let sourceVec = vectorise sourceRow &sourceRow
        let targetVec = vectorise targetRow &targetRow
        for i = 0 to sourceVec.Length - 1 do
            targetVec[i] <- Avx2.Or(sourceVec[i], targetVec[i])
        
        for i = 0 to sourceRow.Length - 1 do
            targetRow[i] <- targetRow[i] ||| sourceRow[i]
    
    target

let inline foldY (grid: Span2D<byte>, col: int) =
    #if DEBUG
    printfn "fold x=%A" col
    for v in grid.GetColumn col do
        if v <> 0uy then
            failwith "bad fold"
    #endif
    let left = grid.Slice(0, 0, grid.Height, col)
    let right = grid.Slice(0, col + 1, grid.Height, grid.Width - (col + 1))
    let target = if left.Width >= right.Width then left else right
    let source = if left.Width >= right.Width then right else left
    for i = 0 to source.Height - 1 do
        let mutable sourceRow = source.GetRowSpan i
        let mutable targetRow = (target.GetRowSpan i).Slice(target.Width - sourceRow.Length, sourceRow.Length)
        let sourceVec = vectorise128 sourceRow &sourceRow
        let targetVec = vectoriseRight128 targetRow &targetRow
        for i = 0 to sourceVec.Length - 1 do
            targetVec[i] <- Avx2.Or(Avx2.Shuffle(sourceVec[sourceVec.Length - 1 - i], Vector128.Create(15uy, 14uy, 13uy, 12uy, 11uy, 10uy, 9uy, 8uy, 7uy, 6uy, 5uy, 4uy, 3uy, 2uy, 1uy, 0uy)), targetVec[i])
        
        for i = 0 to sourceRow.Length - 1 do
            targetRow[i] <- targetRow[i] ||| sourceRow[sourceRow.Length - 1 - i]
    
    target

let inline fold (grid: Span2D<byte>, Fold (axis, position)) =
    match axis with
    | X -> foldX(grid, position)
    | Y -> foldY(grid, position)

let printGrid (grid: Span2D<byte>) =
    for i = 0 to grid.Height - 1 do
        let row = grid.GetRowSpan i
        for b in row do
            printf (if b = 0xffuy then "█" else " ")
        printfn ""

let inline partOne(input: Span<byte>) =
    let struct(points, folds, width, height) = parseInput input

    let memory = NativePtr.stackalloc<byte> (width * height)
    let pointGrid = Span2D<byte>(NativePtr.toVoidPtr memory, height, width, 0)
    for Point(x,y) in points do
        pointGrid[y,x] <- 0xffuy

    let next = fold(pointGrid, folds[0])

    let mutable pointCount = 0
    for i = 0 to next.Height - 1 do
        let mutable row = next.GetRowSpan i
        let rowVector = vectorise row &row
        for vec in rowVector do
            let sum =
                Avx2.SumAbsoluteDifferences(
                    Avx2.Subtract(
                        Vector256<byte>.Zero,
                        vec
                    ),
                    Vector256<byte>.Zero
                ).As<uint16, uint32>()
            let sum = sum.GetElement 0 + sum.GetElement 2 + sum.GetElement 4 + sum.GetElement 6
            pointCount <- pointCount + int sum
        for i = 0 to row.Length - 1 do
            if row[i] = 0xffuy then incr &pointCount

    pointCount

let inline partTwo(input: Span<byte>) =
    let struct(points, folds, width, height) = parseInput input

    let memory = NativePtr.stackalloc<byte> (width * height)
    let mutable pointGrid = Span2D<byte>(NativePtr.toVoidPtr memory, height, width, 0)
    for Point(x,y) in points do
        pointGrid[y,x] <- 0xffuy

    for f in folds do
        pointGrid <- fold(pointGrid, f)

    pointGrid

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadBinary ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo(this.loadBinary ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printGrid (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
