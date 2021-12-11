#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes

let getAdjacent maxX maxY x y =
    [
        for x' = max (x-1) 0 to min maxX (x+1) do
            for y' = max (y-1) 0 to min maxY (y+1) do
                if x <> x' && y <> y' then
                    yield struct(x',y')
    ]

let step arr =
    let getAdjacent = getAdjacent ((Array2D.length1 arr) - 1) ((Array2D.length2 arr) - 1)
    let mutable flashes = []
    let next = 
        arr
        |> Array2D.mapi (fun x y e -> 
                        if e = 9uy then 
                            flashes <- struct(x,y)::flashes
                        e + 1uy)
    
    let mutable flashCount = 0

    while flashes <> List.empty do
        incr &flashCount
        let struct(x,y)::tail = flashes
        flashes <- tail
        let adjacent = getAdjacent x y
        for struct(x', y') in adjacent do
            let updated = next[x',y'] + 1uy
            next[x',y'] <- updated
            if updated = 10uy then
                flashes <- struct(x',y')::flashes
    
    struct(flashCount, Array2D.map (fun e -> if e > 9uy then 0uy else e) next)

let inline partOne(input: Span<byte> inref) =
    let width = input.IndexOf ((byte)'\n')
    let height = input.Length / (width + 1)
    let mutable arr = Array2D.zeroCreate<byte> width height

    for c = 0 to height - 1 do
        let stride = c * (width + 1)
        for r = 0 to width - 1 do
            let i = stride + r
            arr[r,c] <- input[i] - (byte)'0'

    let mutable flashCount = 0
    for i = 1 to 100 do
        let struct(count, next) = step arr
        flashCount <- flashCount + count
        arr <- next
    flashCount

let inline partTwo(input: Span<byte> inref) =
    let width = input.IndexOf ((byte)'\n')
    let height = input.Length / (width + 1)
    let mutable arr = Array2D.zeroCreate<byte> width height

    for c = 0 to height - 1 do
        let stride = c * (width + 1)
        for r = 0 to width - 1 do
            let i = stride + r
            arr[r,c] <- input[i] - (byte)'0'

    let mutable flashCount = 0
    let mutable iterations = 0
    while flashCount <> width * height do
        let struct(count, next) = step arr
        flashCount <- count
        arr <- next
        incr &iterations
    iterations

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        let input = this.loadBinary ()
        partOne(&input)

    [<Benchmark>]
    member public this.PartTwo () =
        let input = this.loadBinary ()
        partTwo(&input)

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
