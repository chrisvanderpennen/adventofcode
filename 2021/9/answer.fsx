#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes

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

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
