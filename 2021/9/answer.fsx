#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading.Tasks

let inline isMinimum maxX maxY (arr: byte[,]) x y elem =
    (x = 0    || arr[x-1, y] > elem) &&
    (x = maxX || arr[x+1, y] > elem) &&
    (y = 0    || arr[x, y-1] > elem) &&
    (y = maxY || arr[x, y+1] > elem)

let getAdjacent maxX maxY x y =
    Set [
        if x > 0 then yield (x-1, y)
        if x < maxX then yield (x+1, y)
        if y > 0 then yield (x, y-1)
        if y < maxY then yield (x, y+1)
    ]

let getAdjacentList maxX maxY x y =
    [
        if x > 0 then yield struct(x-1, y)
        if x < maxX then yield struct(x+1, y)
        if y > 0 then yield struct(x, y-1)
        if y < maxY then yield struct(x, y+1)
    ]

let rec doSearch getAdjacent arr acc visited (stack: Set<int*int>) =
    match stack.Count with
    | 0 -> acc
    | _ ->
        let pos = Set.minElement stack
        let adjacent =
            getAdjacent <|| pos
            |> Set.filter (fun pos -> Array2D.get arr <|| pos <> 9uy)
        let stack = Set.union (Set.difference adjacent visited) stack |> Set.remove pos
        doSearch getAdjacent arr (acc + 1) (Set.add pos visited) stack

let floodSearch (arr: byte[,]) pos =
    let getAdjacent = getAdjacent ((arr.GetLength 0) - 1) ((arr.GetLength 1) - 1)
    doSearch getAdjacent arr 0 Set.empty (Set [pos])

let rec doSearchHashSet getAdjacent arr acc (visited: HashSet<struct(int*int)>) (stack: HashSet<struct(int*int)>) =
    match stack.Count with
    | 0 -> acc
    | _ ->
        let pos = Seq.head stack
        let adjacent =
            StructTuple2.apply getAdjacent pos
            |> Seq.filter (fun pos -> StructTuple2.apply (Array2D.get arr) pos <> 9uy)
            |> HashSet
        adjacent.ExceptWith visited
        stack.UnionWith adjacent
        stack.Remove pos
        visited.Add pos
        doSearchHashSet getAdjacent arr (acc + 1) visited stack

let floodSearchHashSet (arr: byte[,]) pos =
    let getAdjacent = getAdjacentList ((arr.GetLength 0) - 1) ((arr.GetLength 1) - 1)
    doSearchHashSet getAdjacent arr 0 (HashSet()) (HashSet [pos])

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.partOne () =
        let input = this.loadBinary ()
        let width = input.IndexOf ((byte)'\n')
        let height = input.Length / (width + 1)
        let arr = Array2D.zeroCreate<byte> width height

        for c = 0 to height - 1 do
            let stride = c * (width + 1)
            for r = 0 to width - 1 do
                let i = stride + r
                arr[r,c] <- input[i] - (byte)'0'
        
        let isMinimum = isMinimum (width - 1) (height - 1) arr

        let mutable minima = []
        Array2D.iteri (fun x y e -> if isMinimum x y e then minima <- (1 + int e)::minima) arr
        List.sum minima

    [<Benchmark>]
    member public this.partTwo () =
        let input = this.loadBinary ()
        let width = input.IndexOf ((byte)'\n')
        let height = input.Length / (width + 1)
        let arr = Array2D.zeroCreate<byte> width height

        for c = 0 to height - 1 do
            let stride = c * (width + 1)
            for r = 0 to width - 1 do
                let i = stride + r
                arr[r,c] <- input[i] - (byte)'0'
        
        let isMinimum = isMinimum (width - 1) (height - 1) arr

        let mutable minima = []
        Array2D.iteri (fun x y e -> if isMinimum x y e then minima <- (x,y)::minima) arr

        let productOfTop3basinSizes =
            minima
            |> Array.ofList
            |> Array.Parallel.map (floodSearch arr)
            |> Array.sortDescending
            |> Array.take 3
            |> Array.reduce (*)
        
        productOfTop3basinSizes

    [<Benchmark>]
    member public this.partTwoHashSet () =
        let input = this.loadBinary ()
        let width = input.IndexOf ((byte)'\n')
        let height = input.Length / (width + 1)
        let arr = Array2D.zeroCreate<byte> width height

        for c = 0 to height - 1 do
            let stride = c * (width + 1)
            for r = 0 to width - 1 do
                let i = stride + r
                arr[r,c] <- input[i] - (byte)'0'
        
        let isMinimum = isMinimum (width - 1) (height - 1) arr

        // let mutable minima = ConcurrentBag ()
        // Parallel.For(0, arr.GetLength 0, (fun x -> ignore <| Parallel.For(0, arr.GetLength 1, (fun y -> if isMinimum x y arr[x,y] then minima.Add struct(x,y)))))
        let mutable minima = []
        Array2D.iteri (fun x y e -> if isMinimum x y e then minima <- struct(x,y)::minima) arr

        let productOfTop3basinSizes =
            minima
            |> Array.ofSeq
            |> Array.Parallel.map (floodSearchHashSet arr)
            |> Array.sortDescending
            |> Array.take 3
            |> Array.reduce (*)
        
        productOfTop3basinSizes

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())
printfn "%A" (answer.partTwoHashSet ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
