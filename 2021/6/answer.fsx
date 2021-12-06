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
open Microsoft.FSharp.NativeInterop

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

//    [<Benchmark>]
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
        let fish = (Array.zeroCreate<int> 16).AsSpan()
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1

        let fishVectors = MemoryMarshal.Cast<int, Vector256<int>>(fish)
        let shuffleVector = Vector256.Create(1, 2, 3, 4, 5, 6, 7, 0)

        let mutable overflow = 0
        for i = 0 to 79 do
            let temp = fish[0]
            fishVectors[1] <- Avx2.PermuteVar8x32(fishVectors.[0], shuffleVector).WithElement(7, overflow)
            fishVectors[0] <- Avx2.Add (Vector256.Create(0, 0, 0, 0, 0, 0, temp, 0), fishVectors[1])
            overflow <- temp
        
        let sum128 = Avx2.Add(fishVectors[0].GetLower(), fishVectors[0].GetUpper())
        (hsum_epi32_avx sum128) + overflow


//    [<Benchmark>]
    member public _.partTwoSlow () =
        let bytes = loadWithBinary inputBytes
        let mutable fish = Array.zeroCreate<uint64> 9
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1uL
        
        for _ = 0 to 255 do
            fish <- Array.permute (fun i -> (i - 1 + fish.Length) % fish.Length) fish
            fish[6] <- fish[6] + fish[8]
        
        let mutable i = 0uL
        for f in fish do
            i <- i + f
        i

    [<Benchmark>]
    member public _.partTwo () =
        let bytes = loadWithBinary inputBytes
        let fish = (Array.zeroCreate<uint64> 12).AsSpan()
        let fishMiddle = fish.Slice(3)
        let fishRight = fish.Slice(6)
        
        for b = 0 to (bytes.Length / 2) do
            let idx = int(bytes[b <<< 1] &&& mask)
            fish[idx] <- fish[idx] + 1uL

        let fishVectorLeft = MemoryMarshal.Cast<uint64, Vector256<uint64>>(fish)
        let fishVectorMiddle = MemoryMarshal.Cast<uint64, Vector256<uint64>>(fishMiddle)
        let fishVectorRight = MemoryMarshal.Cast<uint64, Vector256<uint64>>(fishRight)

        for i = 0 to 255 do
            let temp = fish[0]
            fishVectorLeft[0] <- Avx2.Permute4x64(fishVectorLeft[0], 0b00111001uy)
            fishVectorMiddle[0] <- Avx2.Permute4x64(fishVectorMiddle[0], 0b00111001uy)
            fishVectorRight[0] <- Avx2.Permute4x64(fishVectorRight[0], 0b11001001uy)
            fish[6] <- fish[6] + temp
        
        let mutable i = 0uL
        for idx = 0 to 8 do
            i <- i + fish[idx]
        i

let answer = Answer ()
#if !INTERACTIVE
answer.setup ()
#endif
printfn "%A" ((answer.partOne ()))
printfn "%A" (answer.partTwo ())

BenchmarkRunner.Run<Answer>() |> ignore
