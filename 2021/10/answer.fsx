#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes
open System.Collections.Generic
open System.Threading.Tasks

[<Struct>]
type ValueResult<'a, 'b> =
| ValueOk of ok: 'a
| ValueError of error: 'b

let [<Literal>] OpenRound = '('
let [<Literal>] OpenSquare = '['
let [<Literal>] OpenCurly = '{'
let [<Literal>] OpenAngle = '<'

let [<Literal>] CloseRound = ')'
let [<Literal>] CloseSquare = ']'
let [<Literal>] CloseCurly = '}'
let [<Literal>] CloseAngle = '>'

let rec findBrokenChunk(line: ReadOnlySpan<char>, listOfOpenings) =
    if line.Length = 0 then
        ValueOk listOfOpenings
    else
        let character = line[0]
        match character with
        | OpenRound
        | OpenSquare
        | OpenCurly
        | OpenAngle ->
            findBrokenChunk (line.Slice 1, character::listOfOpenings)

        | CloseRound ->
            match listOfOpenings with
            | OpenRound::rest -> findBrokenChunk (line.Slice 1, rest)
            | _ -> ValueError character
        | CloseSquare ->
            match listOfOpenings with 
            | OpenSquare::rest -> findBrokenChunk (line.Slice 1, rest) 
            | _ -> ValueError character
        | CloseCurly ->
            match listOfOpenings with 
            | OpenCurly::rest -> findBrokenChunk (line.Slice 1, rest) 
            | _ -> ValueError character
        | CloseAngle ->
            match listOfOpenings with 
            | OpenAngle::rest -> findBrokenChunk (line.Slice 1, rest) 
            | _ -> ValueError character

let inline partOne(input: ReadOnlySpan<char> inref) =
    let mutable lines = input.EnumerateLines ()

    let mutable round = 0
    let mutable square = 0
    let mutable curly = 0
    let mutable angle = 0

    while lines.MoveNext () do
        let line = lines.Current
        match findBrokenChunk(line, []) with
            | ValueError CloseRound -> incr &round
            | ValueError CloseSquare -> incr &square
            | ValueError CloseCurly -> incr &curly
            | ValueError CloseAngle -> incr &angle
            | _ -> ()

    round * 3 + square * 57 + curly * 1197 + angle * 25137

let inline partTwo(input: ReadOnlySpan<char> inref) =
    let mutable lines = input.EnumerateLines ()

    let mutable scores = []

    while lines.MoveNext () do
        let line = lines.Current
        match findBrokenChunk(line, []) with
            | ValueOk tail -> 
                let score =
                    tail |> List.fold ( fun a e -> a * 5L + match e with | OpenRound -> 1L | OpenSquare -> 2L | OpenCurly -> 3L | OpenAngle -> 4 ) 0L
                scores <- score :: scores
            | _ -> ()
    List.sort scores |> List.item (scores.Length / 2)

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        let input = this.loadChars ()
        partOne(&input)

    [<Benchmark>]
    member public this.PartTwo () =
        let input = this.loadChars ()
        partTwo(&input)

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
