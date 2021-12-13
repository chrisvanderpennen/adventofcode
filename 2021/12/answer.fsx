#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes

type Cave =
| Start
| Small of name:string
| Large of name:string
| End

let identifyCave = function
| "start" -> Start
| "end" -> End
| name -> if Char.IsLower name[0] then Small name else Large name

let inline parseLines (input: string list) =
    let outEdges =
        input
        |> List.collect (fun s -> let [|a; b|] = s.Split('-') in let a = identifyCave a in let b = identifyCave b in [a, b; b, a])
        |> List.groupBy fst
        |> List.map (Tuple2.mapSnd (List.map snd >> Set))
        |> Map.ofList
    outEdges

let rec search results visited (edges: Map<Cave, Set<Cave>>) path next =
    match next with
    | End -> [next::path]
    | _ ->
        match edges.TryFind next with
        | Some outEdges ->
            let visited = match next with | Large _ -> visited | _ -> Set.add next visited
            let outEdges =
                Set.difference outEdges visited
            let r = outEdges |> Seq.collect (fun outEdge -> search results visited edges (next::path) outEdge) |> List.ofSeq
            r @ results
        | None ->
            // dead end
            results

let inline partOne(input: string list) =
    let outEdgeList = parseLines input
    let allPaths = search [] (Set [Start]) outEdgeList List.empty Start
    List.length allPaths
    #if DEBUG
    , allPaths
    |> List.map List.rev
    |> List.map (List.map (function | Start -> "start" | End -> "end" | Small c | Large c -> c) >> List.reduce (sprintf "%s,%s"))
    #endif

let rec search2 results visited visitedSmall hasVisitedSecondSmall (edges: Map<Cave, Set<Cave>>) path next =
    match next with
    | End -> [next::path]
    | _ ->
        match edges.TryFind next with
        | Some outEdges ->
            let visited, visitedSmall, hasVisitedSecondSmall = 
                match next with
                | Small _  ->
                    if hasVisitedSecondSmall then Set.add next visited, Set.empty, true
                    else if Set.contains next visitedSmall then Set.union visited visitedSmall, Set.empty, true
                    else visited, Set.add next visitedSmall, false
                | _ -> visited, visitedSmall, hasVisitedSecondSmall
            let outEdges =
                Set.difference outEdges visited
            outEdges |> Seq.collect (fun outEdge -> search2 results visited visitedSmall hasVisitedSecondSmall edges (next::path) outEdge) |> List.ofSeq
        | None ->
            // dead end
            []

let inline partTwo (input: string list) =
    let outEdgeList = parseLines input
    let allPaths = search2 [] (Set [Start]) Set.empty false outEdgeList List.empty Start
    List.length allPaths
    #if DEBUG
    , allPaths
    |> List.map List.rev
    |> List.map (List.map (function | Start -> "start" | End -> "end" | Small c | Large c -> c) >> List.reduce (sprintf "%s,%s"))
    #endif

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadLinesList ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo(this.loadLinesList ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
