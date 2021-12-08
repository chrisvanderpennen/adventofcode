[<AutoOpen>]
module Util
#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#endif

open System
open System.IO
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open BenchmarkDotNet.Attributes
open System.Buffers

[<SimpleJob>]
[<MemoryDiagnoser>]
// [<DisassemblyDiagnoser(printSource = true)>]
type BaseAnswer () =

#if INTERACTIVE
    let inputStr = File.ReadAllText "input.txt"
    let inputBytes = File.ReadAllBytes "input.txt"

    member public this.setup () =
        ()
#else
    let mutable inputStr = ""
    let mutable inputBytes = Array.empty

    [<GlobalSetup>]
    member public this.setup () =
        inputStr <- File.ReadAllText "input.txt"
        inputBytes <- File.ReadAllBytes "input.txt"
#endif

    member public _.loadChars () =
        inputStr.AsSpan()
    
    member public _.loadLinesList () =
        let lines = ResizeArray ()
        let mutable enumerator = inputStr.AsSpan().EnumerateLines()
        while enumerator.MoveNext() do
            lines.Add (String(enumerator.Current))

        let mutable list = []
        for i = lines.Count - 1 downto 0 do
            list <- lines[i]::list
        list

    member public _.loadBinary () = 
        Span(inputBytes)

    member public _.loadReader () =
        SequenceReader (ReadOnlySequence inputBytes)

let output title obj =
    printfn "%s: %A" title obj

let tee obj =
    printfn "%A" obj
    obj

let loadLines f =
    File.ReadLines "input.txt"
    |> Seq.map f
    |> List.ofSeq

module Tuple2 =
    let inline map ([<InlineIfLambda>]f: 'a -> 'b) ([<InlineIfLambda>]g: 'c -> 'd) (a,b) = f a, g b
    let inline mapFst ([<InlineIfLambda>]f) (a, b) = f a, b
    let inline mapSnd ([<InlineIfLambda>]f) (a, b) = a, f b
    let apply = (<||)
    let uncurry f a b = f (a, b)

module Fn =
    let flip f a b = f b a

module Option =
    let inline (>>=) a ([<InlineIfLambda>]f) = Option.bind f a
    let inline (>=>) ([<InlineIfLambda>]f: 'a -> 'b option) ([<InlineIfLambda>]g: 'b -> 'c option) a =
        f a >>= g
    let inline (<=<) (f: 'b -> 'c option) (g: 'a -> 'b option) =
        g >=> f

type ReadOnlySpan<'T> with
    member sp.GetSlice(startIdx, endIdx) =
        let s = defaultArg startIdx 0
        let e = defaultArg endIdx sp.Length
        sp.Slice(s, e - s)

let vectorise (span: Span<'t>) (tail: Span<'t> outref) =
    let vectors = MemoryMarshal.Cast<'t, Vector256<'t>>(span)
    let vSz = vectors.Length * sizeof<'t> * Vector256<'t>.Count
    if vSz < span.Length then
        tail <- span.Slice(vectors.Length * sizeof<'t> * Vector256<'t>.Count)
    vectors

let vectorise128 (span: Span<'t>) (tail: Span<'t> outref) =
    let vectors = MemoryMarshal.Cast<'t, Vector128<'t>>(span)
    tail <- span.Slice(vectors.Length * sizeof<'t> * Vector128<'t>.Count)
    vectors

module Array =
    let inline reduceInline ([<InlineIfLambda>]f: 'a -> 'a -> 'a) (arr: 'a[]) =
        let mutable res = arr.[0]
        for i = 1 to arr.Length-1 do
            res <- f res arr.[i]
        res

    let inline permuteInline ([<InlineIfLambda>]indexMap) (arr : _[]) =
        let res = Array.zeroCreate arr.Length
        let inv = Array.zeroCreate arr.Length
        for i = 0 to arr.Length - 1 do
            let j = indexMap i
            if j < 0 || j >= arr.Length then invalidArg "indexMap" "not a permutation"
            res.[j] <- arr.[i]
            inv.[j] <- 1uy
        for i = 0 to arr.Length - 1 do
            if inv.[i] <> 1uy then invalidArg "indexMap" "not a permutation"
        res
