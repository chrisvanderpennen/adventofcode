#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.partOne_FirstAttempt () =
        let input = this.loadBinary ()
        let arr = Array.zeroCreate 1000

        let mutable i = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        let arr2 = Array.sort arr
        let median = arr2[arr.Length / 2]
        let diff = Array.sumBy (fun i -> (median - i) |> abs) arr2
        diff

    [<Benchmark>]
    member public this.partOne () =
        let input = this.loadBinary ()
        let arr = Array.zeroCreate 1000

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        let inline riemannDistance a b = abs(b - a)

        let rec frechetMedian struct(dMin, pMin) struct(dMax, pMax) =
            match abs(pMax - pMin) with
            | 0
            | 1 -> if dMin < dMax then pMin else pMax
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (riemannDistance p1) arr
                let pivot = struct(d1, p1)
                let wall = min struct(dMin, pMin) struct(dMax, pMax)
                frechetMedian pivot wall

        let fm = frechetMedian struct((Array.sumBy (riemannDistance minV) arr), minV) struct((Array.sumBy (riemannDistance maxV) arr), maxV)
        let distance = Array.sumBy (riemannDistance fm) arr
        fm, distance

    [<Benchmark>]
    member public this.partTwo () =
        let input = this.loadBinary ()
        let arr = Array.zeroCreate 1000

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        let inline riemannDistance a b =
            let dst = abs(b - a)
            dst * (dst + 1) / 2

        let rec frechetMedian struct(dMin, pMin) struct(dMax, pMax) =
            match abs(pMax - pMin) with
            | 0
            | 1 -> if dMin < dMax then pMin else pMax
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (riemannDistance p1) arr
                let pivot = struct(d1, p1)
                let wall = min struct(dMin, pMin) struct(dMax, pMax)
                frechetMedian pivot wall

        let fm = frechetMedian struct((Array.sumBy (riemannDistance minV) arr), minV) struct((Array.sumBy (riemannDistance maxV) arr), maxV)
        let distance = Array.sumBy (riemannDistance fm) arr
        fm, distance

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
