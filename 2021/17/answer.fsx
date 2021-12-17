#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices

let x1 = 137
let x2 = 171
let y1 = -73
let y2 = -98

let trinum n = n * (n + 1) / 2

let between (x: int) l u = x >= l && x <= u

// --- part one ---
// one goes up, then returns to 0 at -v
// therefore after returning to 0, one is at y=-(v + 1)
let inline partOne() = trinum 97

let intersectsGoal x y = between x x1 x2 && between y y2 y1
let overshot x y = x > x2 || y < y2

let rec findMinXvel i =
    if trinum i > x1 then i else findMinXvel (i + 1)

let mutable nextXvelTable = Array.init (x2 + 1) (fun i -> if i = 0 then 0 else i - 1)
let initModule () =
    nextXvelTable <- Array.init (x2 + 1) (fun i -> if i = 0 then 0 else i - 1)

[<MethodImpl(MethodImplOptions.AggressiveInlining)>]
let rec testPath xvel yvel x y =
    let x = x + xvel
    let y = y + yvel
    if intersectsGoal x y then true
    else if overshot x y then false
    else
        let nextXvel = nextXvelTable[xvel]
        let nextYvel = yvel - 1
        testPath nextXvel nextYvel x y

// --- part two ---
// our y velocity is bounded by y2 .. (abs y2) - 1
// our x velocity is bounded by x2 .. (the smallest triangle number that produces x1)
let inline partTwo() =
    let minXvel = findMinXvel 0
    let maxXvel = x2
    let minYvel = y2
    let maxYvel = (abs y2) - 1

    let mutable count = 0

    for x = minXvel to maxXvel do
        for y = minYvel to maxYvel do
            if testPath x y 0 0 then incr &count

    count

[<RyuJitX64Job>]
[<MemoryDiagnoser>]
[<DisassemblyDiagnoser(printSource = true, maxDepth=3, exportHtml=true)>]
type Answer () =

    [<GlobalSetup>]
    member public _.Setup () = initModule ()

    //[<Benchmark>]
    member public this.PartOne () =
        partOne()

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo()

let answer = Answer ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
