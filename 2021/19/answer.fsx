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
    pi * -0.5f
]

let facings = [
    Quaternion.CreateFromYawPitchRoll(0f, 0f, 0f)
    Quaternion.CreateFromYawPitchRoll(pi * 0.5f, 0f, 0f)
    Quaternion.CreateFromYawPitchRoll(pi, 0f, 0f)
    Quaternion.CreateFromYawPitchRoll(pi * -0.5f, 0f, 0f)
    Quaternion.CreateFromYawPitchRoll(0f, pi * 0.5f, 0f)
    Quaternion.CreateFromYawPitchRoll(0f, pi * -0.5f, 0f)
]

let attitudes = angles |> List.map (fun angle -> Quaternion.CreateFromYawPitchRoll(0f, 0f, angle))

let allRotations = List.allPairs facings attitudes |> List.map (fun (facing, attitude) -> facing * attitude)

let manhattanDistance (a: Vector3) (b: Vector3) =
    abs(b.X - a.X) + abs(b.Y - a.Y) + abs(b.Z - a.Z)
    |> int

let flipNegativeZero = function | -0f -> 0f | f -> f

let roundVector (v: Vector3) =
    // for some reason y ends up -0 in a few cases.  floats, right
    Vector3(MathF.Round(v.X), flipNegativeZero <| MathF.Round(v.Y), MathF.Round(v.Z))

let transformRound (quat: Quaternion) vec =
    roundVector <| Vector3.Transform(vec, quat)

type Scanner = {
    Coordinates: HashSet<Vector3>
    DistanceSets: HashSet<int>[]
    Origin: Vector3
}

type ScannerData = {
    RotatedCoordinateSets: HashSet<Vector3>[]
    DistanceSets: HashSet<int>[]
}

let countOverlaps (a: HashSet<_>) b =
    b |> Seq.filter (a.Contains) |> Seq.length

let calculateDistanceSets vectors =
    vectors
    |> Seq.map (fun v -> Seq.map (fun vv -> manhattanDistance v vv) (Seq.filter (fun vv -> vv <> v) vectors) |> HashSet)
    |> Array.ofSeq

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
    {
        Coordinates=HashSet initialVectors
        DistanceSets=calculateDistanceSets initialVectors
        Origin=Vector3.Zero
    }, next

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
        |> Seq.map (fun quat -> (Seq.map (transformRound quat) initialVectors) |> HashSet)
        |> Array.ofSeq

    {
        RotatedCoordinateSets = allRotatedVectors;
        DistanceSets = calculateDistanceSets initialVectors
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

    #if DEBUG
    printfn "%A %A" (List.length origins) (List.length scannerData)
    #endif

    match scannerData with
    | [] -> origins
    | _ ->
        let alignedScanners, tail =
            ((origins,[]), scannerData)
            ||> List.fold (
                fun (hits, misses) data ->
                    hits
                    |> List.tryFind (fun origin -> hasDistanceOverlap origin.DistanceSets data.DistanceSets)
                    |> Option.bind (fun origin -> 
                        data.RotatedCoordinateSets
                        |> Array.tryPick (fun set ->
                            Seq.allPairs (origin.Coordinates) set
                            |> Seq.map (Tuple2.apply (-))
                            |> Seq.tryPick (fun offset -> 
                                let translatedSet = set |> Seq.map (fun c -> c + offset) |> HashSet
                                if translatedSet |> Seq.filter (origin.Coordinates.Contains) |> Seq.length >= 12 then
                                    Some (translatedSet, offset)
                                else
                                    None
                                )
                            |> Option.map (fun (translatedSet, offset) -> (data, translatedSet, offset))
                        )
                        |> Option.map (fun (overlap, coords, offset) ->
                            {
                                Coordinates = coords
                                DistanceSets = overlap.DistanceSets
                                Origin = -offset
                            }::hits, misses
                        )
                    )
                    |> Option.defaultValue (hits, (data::misses))
            )

        alignScanners alignedScanners tail

let inline solve(input: string list) =
    let firstOrigin,tail = parseScanner input
    let scannerData = parseScanners tail
    let origins = [firstOrigin]

    let alignedScanners = alignScanners origins scannerData
    let allPoints = HashSet()
    alignedScanners |> List.iter (fun s -> allPoints.UnionWith s.Coordinates)
    let offsets = alignedScanners |> List.map (fun s -> s.Origin)
    let maxDistance = List.allPairs offsets offsets |> List.map (fun (a,b) -> manhattanDistance a b) |> List.max
    allPoints.Count, maxDistance

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.Solve () =
        solve(this.loadLinesList ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.Solve ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif

// this makes static initialisers run
[<EntryPoint>]
let main _ =
    0
