#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#r "nuget:Microsoft.Toolkit.HighPerformance"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open Microsoft.Toolkit.HighPerformance
open BenchmarkDotNet.Attributes

let parseInput (input: ReadOnlySpan<char>) =
    let firstLine = input.Slice(0, input.IndexOf('\n')).ToArray()
    let initialPairs = Array.pairwise firstLine |> Array.countBy id |> Array.map (Tuple2.mapSnd int64)

    let mappings = input.Slice(firstLine.Length + 2)
    let mappingCount = mappings.Count('\n') + 1
    let mappingArray = Array.zeroCreate mappingCount
    let mutable i = 0
    for mapping in mappings.Tokenize('\n') do
        mappingArray[i] <- (mapping[0], mapping[1]), mapping[6]
        i <- i + 1
    
    let pairTable = Map.ofArray initialPairs
    
    let mutable characterTable = Map.empty
    for c in firstLine do
        characterTable <- characterTable |> Map.change c (Some << function | Some i -> i + 1L | None -> 1L)

    pairTable, characterTable, mappingArray

let iterate mappingArray pairTable characterTable =
    ((Map.empty, characterTable), mappingArray)
    ||> Array.fold
        (fun (acc, chars) (source, inserted) ->
            match Map.tryFind source pairTable with
            | None -> acc, chars
            | Some count ->
                acc
                |> Map.change (fst source, inserted) (Some << function | Some current -> current + count | None -> count)
                |> Map.change (inserted, snd source) (Some << function | Some current -> current + count | None -> count)
                , chars |> Map.change inserted (Option.defaultValue 0L >> (+) count >> Some)
        )
    
let inline partOne(input: ReadOnlySpan<char>) =
    let pairTable, characterTable, mappingArray = parseInput(input)

    let mutable state = (pairTable, characterTable)
    for i = 1 to 10 do
        state <- iterate mappingArray <|| state
    
    let (_, counts) = state

    let min = Seq.minBy snd (Map.toSeq counts)
    let max = Seq.maxBy snd (Map.toSeq counts)

    (snd max) - (snd min)

let inline partTwo(input: ReadOnlySpan<char>) =
    let pairTable, characterTable, mappingArray = parseInput(input)

    let mutable state = (pairTable, characterTable)
    for i = 1 to 40 do
        state <- iterate mappingArray <|| state
    
    let (_, counts) = state

    let min = Seq.minBy snd (Map.toSeq counts)
    let max = Seq.maxBy snd (Map.toSeq counts)

    ((snd max) - (snd min)), Seq.sum counts.Values

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadChars ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadChars ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
