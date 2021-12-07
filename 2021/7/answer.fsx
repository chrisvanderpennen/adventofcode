#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86

type Answer () =
    inherit BaseAnswer()

    //[<Benchmark>]
    member public this.countCommas () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

    // [<Benchmark>]
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

        Array.sortInPlace arr
        let median = arr[arr.Length / 2]
        let diff = Array.sumBy (fun i -> (median - i) |> abs) arr
        diff

    [<Benchmark>]
    member public this.partOne () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let arr = Array.zeroCreate (commas + 1)

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

        let rec frechetMedian dMin pMin dMax pMax =
            match abs(pMax - pMin) with
            | 0
            | 1 -> if dMin < dMax then dMin, pMin else dMax, pMax
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (riemannDistance p1) arr
                if dMin < dMax then frechetMedian dMin pMin d1 p1 else frechetMedian dMax pMax d1 p1

        let distance, fm = frechetMedian (Array.sumBy (riemannDistance minV) arr) minV (Array.sumBy (riemannDistance maxV) arr) maxV
        fm, distance

    [<Benchmark>]
    member public this.partTwo () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let arr = Array.zeroCreate (commas + 1)

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

        let rec frechetMedian dMin pMin dMax pMax =
            match abs(pMax - pMin) with
            | 0
            | 1 -> 
                if dMin < dMax then
                    struct(dMin, pMin)
                else 
                    struct(dMax, pMax)
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (riemannDistance p1) arr
                if dMin < dMax then frechetMedian dMin pMin d1 p1 else frechetMedian dMax pMax d1 p1

        let struct(distance, fm) = frechetMedian (Array.sumBy (riemannDistance minV) arr) minV (Array.sumBy (riemannDistance maxV) arr) maxV
        fm, distance, (double (Array.sum arr) / double arr.Length)

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

[<SimpleJob>]
type foo () =
    let mutable arr = Array.empty
    [<GlobalSetup>]
    member public this.setup () = 
        let random = Random ()
        arr <- Array.init 1000 (fun _ -> random.Next ())

    [<Benchmark>]
    member public this.ArraySort () = Array.Sort(arr)

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
