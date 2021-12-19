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
    |> Seq.exists (fun (a,b) -> countOverlaps a b >= 11)

let rec alignScanners (origins: Scanner list) (scannerData: ScannerData list) =

    printfn "%A %A" (List.length origins) (List.length scannerData)

    match scannerData with
    | [] -> origins
    | _ ->
        let overlaps, tail =
            (([],[]), scannerData)
            ||> List.fold (
                fun (hits, misses) data ->
                    origins
                    |> List.tryFind (fun origin -> hasDistanceOverlap origin.DistanceSets data.DistanceSets)
                    |> Option.bind (fun origin -> 
                        data.RotatedCoordinateSets
                        |> Seq.tryPick (fun set ->
                            // TODO
                            // for each origin coordinate
                            // select a target coordinate
                            // offset all target coordinates by (origin - target)
                            // if there are 12 or more offset coordinates in the origin coordinate set
                            // we have found our offset
                            Seq.allPairs (origin.Coordinates) set
                            |> Seq.groupBy (fun (a, b) -> manhattanDistance a b)
                            |> Seq.tryFind (fun (_, set) -> Seq.length set >= 12)
                            |> Option.map (fun (_,matches) -> 
                                let (a,b) = Seq.head matches
                                (origin, data, set, b - a))
                        )
                        |> Option.map (fun (origin, overlap, coords, offset) ->
                            {
                                Coordinates = coords |> Seq.map (fun c -> c - offset) |> Array.ofSeq
                                DistanceSets = overlap.DistanceSets
                            }::hits, misses
                        )
                    )
                    |> Option.defaultValue (hits, (data::misses))
            )

        // let alignedOverlaps =
        //     overlaps
        //     |> List.choose (fun (origin, overlap) ->
        //         overlap.RotatedCoordinateSets
        //         |> Seq.tryPick (fun set ->
        //             Seq.allPairs (origin.Coordinates) set
        //             |> Seq.map (fun (a, b) -> roundVector (b - a))
        //             |> Seq.countBy id
        //             |> Seq.tryFind (fun (_, b) -> b >= 12)
        //             |> Option.map (fun (a,_) -> origin, overlap, set, a)
        //         )
        //     )
        // if alignedOverlaps = [] then failwith "error 2"
        // let alignedOverlaps =
        //     alignedOverlaps
        //     |> List.map (
        //         fun (origin, overlap, coords, offset) -> 
        //             // printfn "--- MATCH ---"
        //             // printfn "%A" (origin.Coordinates |> Array.sortBy (fun v -> v.X, v.Y, v.Z))
        //             // printfn "%A" offset
        //             // printfn "%A" (coords |> Seq.map (fun c -> roundVector(c - offset)) |> Array.ofSeq |> Array.sortBy (fun v -> v.X, v.Y, v.Z))
        //             // printfn "--- ----- ---"
        //             {
        //                 Coordinates = coords |> Seq.map (fun c -> roundVector(c - offset)) |> Array.ofSeq
        //                 DistanceSets = overlap.DistanceSets
        //             }
        //         )

        alignScanners (overlaps @ origins) tail

let inline partOne(input: string list) =
    let firstOrigin,tail = parseScanner input
    let scannerData = parseScanners tail
    let origins = [firstOrigin]

    let alignedScanners = alignScanners origins scannerData
    let allPoints = 
        (HashSet(), alignedScanners)
        ||> Seq.fold (fun set scanner -> set.UnionWith (scanner.Coordinates); set)
    allPoints.Count

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
