#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open System.Runtime.InteropServices
open BenchmarkDotNet.Attributes

let inline hsum_epi32_avx (v: Vector128<int>) =
    let sum64 = Avx2.Add(Avx2.UnpackHigh(v.AsInt64(), v.AsInt64()).AsInt32(), v)
    let sum32 = Avx2.Add(sum64, Avx2.Shuffle(sum64, 0b10110001uy));
    sum32.ToScalar<int> ()

type Answer () =
    inherit BaseAnswer()
    let mask = 0b0000_1111uy

    member this.computeFish days =
        let bytes = this.loadBinary ()
        let mutable tail = Span<byte>()
        let iv = vectorise bytes &tail
        
        let mutable v1 = Vector256<byte>.Zero
        let mutable v2 = Vector256<byte>.Zero
        let mutable v3 = Vector256<byte>.Zero
        let mutable v4 = Vector256<byte>.Zero
        let mutable v5 = Vector256<byte>.Zero

        for i = 0 to iv.Length - 1 do
            let v = iv[i]
            v1 <- Avx2.Subtract(v1, Avx2.CompareEqual(v, Vector256.Create(byte '1')))
            v2 <- Avx2.Subtract(v2, Avx2.CompareEqual(v, Vector256.Create(byte '2')))
            v3 <- Avx2.Subtract(v3, Avx2.CompareEqual(v, Vector256.Create(byte '3')))
            v4 <- Avx2.Subtract(v4, Avx2.CompareEqual(v, Vector256.Create(byte '4')))
            v5 <- Avx2.Subtract(v5, Avx2.CompareEqual(v, Vector256.Create(byte '5')))
        
        let inline vecsum (v: Vector256<byte>) =
            let sabs = Avx2.SumAbsoluteDifferences(v, Vector256.Zero).As<uint16, uint64>()
            sabs.GetElement(0) + sabs.GetElement(1) + sabs.GetElement(2) + sabs.GetElement(3)

        let inline vecsum128 (v: Vector128<byte>) =
            let sabs = Avx2.SumAbsoluteDifferences(v, Vector128.Zero).As<uint16, uint64>()
            sabs.GetElement(0) + sabs.GetElement(1)// + sabs.GetElement(2) + sabs.GetElement(3)

        let fish = (Array.zeroCreate<uint64> 9).AsSpan()

        fish[1] <- vecsum v1
        fish[2] <- vecsum v2
        fish[3] <- vecsum v3
        fish[4] <- vecsum v4
        fish[5] <- vecsum v5
        
        for b = 0 to (tail.Length / 2) do
            let idx = int(tail[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1uL
        
        for i = 0 to (days - 1) do
            let d = (i + 7) % 9
            fish[d] <- fish[d] + fish[i % 9]
        
        let mutable i = 0uL
        for idx = 0 to 8 do
            i <- i + fish[idx]
        i

    [<Benchmark>]
    member public this.partOne () = this.computeFish 80

    [<Benchmark>]
    member public this.partTwo () = this.computeFish 256

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
