#load "../util.fsx"

open System.Collections.Concurrent
open System
type Point = (struct(int * int))
type Line = (struct(Point*Point))

let isOrtho struct(x1,y1) struct(x2,y2) = x1 = x2 || y1 = y2

let parsePoint (l: ReadOnlySpan<char>) =
    let comma = l.IndexOf ','
    let x1 = Int32.Parse(l.Slice(0, comma))
    let x2 = Int32.Parse(l.Slice(comma + 1))
    struct(x1, x2)

let parseLine (l: ReadOnlySpan<char>) =
    let left = l.Slice(0, (l.IndexOf ' '))
    let right = l.Slice(l.LastIndexOf ' ' + 1)
    struct(parsePoint left, parsePoint right)

let part1 () =

    let mutable grid = Map.empty

    let mutable lines = (Util.loadWithChars ()).EnumerateLines()
    while (lines.MoveNext()) do
        let struct(a,b) = parseLine lines.Current
        if isOrtho a b then
            let struct(x1,y1) = a
            let struct(x2,y2) = b
            for x in (min x1 x2)..(max x1 x2) do
                for y in (min y1 y2)..(max y1 y2) do
                    grid <- Map.change struct(x,y) (Option.defaultValue 0 >> ((+) 1) >> Some) grid

    let overlapCount = grid |> Seq.filter (fun k -> k.Value > 1) |> Seq.length
    overlapCount

printfn "%A" (part1 ())

let part2 () =

    let mutable grid = Map.empty

    let mutable lines = (Util.loadWithChars ()).EnumerateLines()
    while (lines.MoveNext()) do
        let struct(a,b) = parseLine lines.Current
        let struct(x1,y1) = a
        let struct(x2,y2) = b
        if isOrtho a b then
            for x in (min x1 x2)..(max x1 x2) do
                for y in (min y1 y2)..(max y1 y2) do
                    grid <- Map.change struct(x,y) (Option.defaultValue 0 >> ((+) 1) >> Some) grid
        else
            Seq.iter2
                (fun x y -> grid <- Map.change struct(x,y) (Option.defaultValue 0 >> ((+) 1) >> Some) grid)
                [x1..(if x2 < x1 then (-1) else 1)..x2]
                [y1..(if y2 < y1 then (-1) else 1)..y2]
        
    let overlapCount = grid |> Map.filter (fun _ v -> v > 1) |> Map.count
    overlapCount

printfn "%A" (part2 ())