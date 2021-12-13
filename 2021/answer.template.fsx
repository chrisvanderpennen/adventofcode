#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes

let inline partOne(input: Span<byte>) =
    ()

let inline partTwo(input: Span<byte>) =
    ()

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadBinary ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadBinary ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
