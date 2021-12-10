#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes

let inline partOne(input: Span<byte> inref) =
    ()

let inline partTwo(input: Span<byte> inref) =
    ()

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
