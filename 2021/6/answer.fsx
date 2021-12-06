#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif
open System
open System.IO
open System.Numerics
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open System.Runtime.InteropServices

let inline hsum_epi32_avx (v: Vector128<int>) =
    let sum64 = Avx2.Add(Avx2.UnpackHigh(v.AsInt64(), v.AsInt64()).AsInt32(), v)
    let sum32 = Avx2.Add(sum64, Avx2.Shuffle(sum64, 0b10110001uy));
    sum32.ToScalar<int> ()

[<SimpleJob>]
[<MemoryDiagnoser>]
type Answer () =
    let mask = 0b0000_1111uy

#if INTERACTIVE
    let inputBytes = ()
#else
    let mutable inputBytes = Array.empty<byte>
    [<GlobalSetup>]
    member public this.setup () =
        inputBytes <- File.ReadAllBytes "input.txt"
#endif

    member public _.wat () =
        let bytes = loadWithBinary inputBytes
        let mutable fishSlow = Array.zeroCreate<int> 9
        let fish = (Array.zeroCreate<int> (512/16)).AsSpan()
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fishSlow[idx] <- fishSlow[idx] + 1
            fish[idx] <- fish[idx] + 1

        let mutable fishVectors = MemoryMarshal.Cast<int, Vector256<int>>(fish)
        let shuffleVector = Vector256.Create(1, 2, 3, 4, 5, 6, 7, 0)

        let mutable overflow = 0
        for _ = 0 to 79 do
            let temp = fishVectors[0].GetElement 0
            fishVectors[1] <- Avx2.PermuteVar8x32(fishVectors.[0], shuffleVector).WithElement(7, overflow)
            fishVectors[0] <- Avx2.Add (Vector256.Create(0, 0, 0, 0, 0, 0, temp, 0), fishVectors[1])
            overflow <- temp

            fishSlow <- Array.permute (fun i -> (i - 1 + fishSlow.Length) % fishSlow.Length) fishSlow
            fishSlow[6] <- fishSlow[6] + fishSlow[8]
            printfn "%A" fishSlow
            printfn "%A %A" fishVectors[0] overflow
        
        let mutable i = 0
        for f in fishSlow do
            i <- i + int f
        i

    [<Benchmark>]
    member public _.partOneSlow () =
        let bytes = loadWithBinary inputBytes
        let mutable fish = Array.zeroCreate<int> 9
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1
        
        for _ = 0 to 79 do
            fish <- Array.permute (fun i -> (i - 1 + fish.Length) % fish.Length) fish
            fish[6] <- fish[6] + fish[8]
        
        let mutable i = 0
        for f in fish do
            i <- i + int f
        i

    [<Benchmark>]
    member public _.partOne () =
        let bytes = loadWithBinary inputBytes
        let fish = (Array.zeroCreate<int> (512/16)).AsSpan()
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1

        let mutable fishVectors = MemoryMarshal.Cast<int, Vector256<int>>(fish)
        let shuffleVector = Vector256.Create(1, 2, 3, 4, 5, 6, 7, 0)

        let mutable overflow = 0
        for i = 0 to 79 do
            let temp = fish[0]
            fishVectors[1] <- Avx2.PermuteVar8x32(fishVectors.[0], shuffleVector).WithElement(7, overflow)
            fishVectors[0] <- Avx2.Add (Vector256.Create(0, 0, 0, 0, 0, 0, temp, 0), fishVectors[1])
            overflow <- temp
        
        let sum128 = Avx2.Add(fishVectors[0].GetLower(), fishVectors[0].GetUpper())
        (hsum_epi32_avx sum128) + overflow

let answer = Answer ()
#if !INTERACTIVE
answer.setup ()
#endif
//answer.wat ()
printfn "%A" ((answer.partOneSlow ()))
printfn "%A" ((answer.partOne ()))
//printfn "%A" answer.partTwo ()

BenchmarkRunner.Run<Answer>() |> ignore

//