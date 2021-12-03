open System
open System.IO
open System.Numerics
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type ReadOnlySpan<'T> with
    member sp.GetSlice(startIdx, endIdx) =
        let s = defaultArg startIdx 0
        let e = defaultArg endIdx sp.Length
        sp.Slice(s, e - s)

module Array =
    let inline reduceInline ([<InlineIfLambda>]f: 'a -> 'a -> 'a) (arr: 'a[]) =
        let mutable res = arr.[0]
        for i = 1 to arr.Length-1 do
            res <- f res arr.[i]
        res

open System.Runtime.InteropServices

[<SimpleJob>]
[<MemoryDiagnoser>]
type DayThree () =
    let text = (File.ReadAllText "input.txt").ToCharArray() |> Array.map uint16
    let lines = text.Length / 13

    let goals = Vector<uint16>(Array.create Vector<uint16>.Count ((uint16)lines))
    let double = Vector<uint16>(Array.create Vector<uint16>.Count (2us))
    let mutable counts = Vector<uint16>.Zero
    let result = Array.zeroCreate<uint16>(Vector<uint16>.Count)

    [<Benchmark>]
    member public _.partOne () =
        let buffer = Array.zeroCreate<uint16>(Vector<uint16>.Count).AsSpan()
        let mutable rest = ReadOnlySpan(text)
        for _ in 0..(lines - 1) do
            let current = 
                if rest.Length < Vector<uint16>.Count 
                then 
                    rest.CopyTo (buffer)
                    MemoryMarshal.Cast<uint16, Vector<uint16>>(buffer)[0]
                else
                    let next = MemoryMarshal.Cast<uint16, Vector<uint16>>(rest)[0]
                    rest <- rest.[13..]
                    next
            let vector = current &&& Vector<uint16>.One
            counts <- counts + vector

        let res = Vector.ConditionalSelect(Vector.GreaterThan (counts * double, goals), Vector<uint16>.One, Vector<uint16>.Zero)
        res.CopyTo result

        let mostCommonNumber = Array.reduceInline (fun l r -> (l <<< 1) + r) result[..11]
        let leastCommonNumber = mostCommonNumber ^^^ 0b0000_1111_1111_1111us
        (int mostCommonNumber) * (int leastCommonNumber)

printfn "%A" ((new DayThree()).partOne())

BenchmarkRunner.Run<DayThree>() |> ignore