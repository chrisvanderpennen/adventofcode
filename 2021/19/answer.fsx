#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes
open System.Numerics
open System.Collections.Generic

let pi = MathF.PI

let angles = [
    0.0f
    pi * 0.5f
    pi
    pi * 1.5f
]
let axes = [
    Vector3.UnitX
    Vector3.UnitY
    Vector3.UnitZ
    -Vector3.UnitX
    -Vector3.UnitY
    -Vector3.UnitZ
]

let allRotations =
    List.allPairs axes angles
    |> List.map Quaternion.CreateFromAxisAngle

let manhattanDistance (a: Vector3) (b: Vector3) =
    abs(b.X - a.X) + abs(b.Y - a.Y) + abs(b.Z - a.Z)
    |> int

let roundVector (v: Vector3) =
    Vector3(MathF.Round(v.X), MathF.Round(v.Y), MathF.Round(v.Z))

let transformRound (quat: Quaternion) vec =
    roundVector <| Vector3.Transform(vec, quat)

type Scanner = {
    Coordinates: Vector3[]
    DistanceSets: HashSet<int>[]
}

type ScannerData = {
    RotatedCoordinateSets: HashSet<Vector3>[]
    DistanceSets: HashSet<int>[]
}

let countOverlaps (a: HashSet<_>) b =
    b |> Seq.filter (a.Contains) |> Seq.length

let parseScanner (lines: string list) =
    let id::vectors = lines
    let rec readVectors acc lines =
        match lines with
        | [] -> acc, []
        | ""::tail -> acc, tail
        | vectorData::tail -> 
            let [|x;y;z|] = vectorData.Split(',') |> Array.map float32
            readVectors (Vector3(x, y, z)::acc) tail
    let initialVectors, next = readVectors [] vectors
    let distanceSets =
        initialVectors
        |> List.map (fun v -> Seq.map (fun vv -> manhattanDistance v vv) (Seq.filter ((<>)v) initialVectors) |> HashSet)
        |> Array.ofList
    {Coordinates=Array.ofList initialVectors; DistanceSets=distanceSets}, next

let parseScannerData (lines: string list) =
    let id::vectors = lines
    let rec readVectors acc lines =
        match lines with
        | [] -> acc, []
        | ""::tail -> acc, tail
        | vectorData::tail -> 
            let [|x;y;z|] = vectorData.Split(',') |> Array.map float32
            readVectors (Vector3(x, y, z)::acc) tail
    let initialVectors, next = readVectors [] vectors

    let allRotatedVectors =
        allRotations
        |> List.map (fun quat -> (Seq.map (transformRound quat) initialVectors) |> HashSet)
        |> Array.ofList

    let distanceSets =
        initialVectors
        |> List.map (fun v -> Seq.map (fun vv -> manhattanDistance v vv) (Seq.filter ((<>)v) initialVectors) |> HashSet)
        |> Array.ofList

    {
        RotatedCoordinateSets = allRotatedVectors;
        DistanceSets = distanceSets
    }, next

let parseScanners input =
    let rec parse acc input =
        match input with
        | [] -> acc
        | lines ->
            let data, next = parseScannerData lines
            parse (data::acc) next
    parse [] input

let hasDistanceOverlap (a: HashSet<int>[]) (b: HashSet<int>[]) =
    Seq.allPairs a b
    |> Seq.exists (fun (a,b) -> countOverlaps a b >= 12)

let rec positionScannerData (origin: Scanner) scannerData =

    let overlap = 
        scannerData
        |> List.find (fun data -> hasDistanceOverlap origin.DistanceSets data.DistanceSets)

    overlap.RotatedCoordinateSets
    |> Seq.pick (fun set ->
        Seq.allPairs (origin.Coordinates) set
        |> Seq.map ((<||) (-) >> roundVector)
        |> Seq.countBy id
        |> Seq.tryFind (snd >> (>=) 12)
        |> Option.map (fun (a,_) -> set, a)
    )

let inline partOne(input: string list) =
    let origin,tail = parseScanner input
    let scannerData = parseScanners tail

    let overlaps = 
        scannerData
        |> List.filter (fun data -> hasDistanceOverlap origin.DistanceSets data.DistanceSets)
    
    overlaps
    |> List.map (fun overlap ->
        overlap.RotatedCoordinateSets
        |> Seq.pick (fun set ->
            Seq.allPairs (origin.Coordinates) set
            |> Seq.map ((<||) (-) >> roundVector)
            |> Seq.countBy id
            |> Seq.tryFind (snd >> (>=) 12)
            |> Option.map (fun (a,_) -> set, a)
        )
    )

let inline partTwo(input: Span<byte>) =
    ()

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadLinesList ())

    //[<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadBinary ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif

// this makes static initialisers run
[<EntryPoint>]
let main _ =
    0
