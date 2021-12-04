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

let loadWithBinary () = 
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