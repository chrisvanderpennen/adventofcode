#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.partOne () =
        ()

    [<Benchmark>]
    member public this.partTwo () =
        ()

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
