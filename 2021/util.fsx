[<AutoOpen>]
module Util

open System.IO
open System

let inputStr = File.ReadAllText "input.txt"
let inputBytes = File.ReadAllBytes "input.txt"

let loadLines fn = 
    inputStr.Split '\n'
    |> Seq.map fn
    |> List.ofSeq

#if !INTERACTIVE
let loadWithChars inputStr =
#else
let loadWithChars () =
#endif
    inputStr.AsSpan()

#if !INTERACTIVE
let loadWithBinary inputBytes = 
#else
let loadWithBinary () = 
#endif
    ReadOnlySpan(inputBytes)

let output title obj =
    printfn "%s: %A" title obj

let dump obj =
    printfn "%A" obj
    obj

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

module Array =
    let inline reduceInline ([<InlineIfLambda>]f: 'a -> 'a -> 'a) (arr: 'a[]) =
        let mutable res = arr.[0]
        for i = 1 to arr.Length-1 do
            res <- f res arr.[i]
        res
