#if DEBUG
open Checked
#endif

open BenchmarkDotNet.Attributes

let inline getVirtualisedValue currentVirtualisedValue (memory: int[,]) row col =
    if row < 0 || row >= Array2D.length1 memory || col < 0 || col >= Array2D.length2 memory then
        currentVirtualisedValue
    else
        memory[row, col]

let inline nextVirtualisedValue (lookupTable: int[]) currentVirtualisedValue =
    if currentVirtualisedValue = 0 then lookupTable[0] else lookupTable[0b1_1111_1111]

let inline lookupValue currentVirtualisedValue (lookupTable: int[]) (memory: int[,]) row col =
    let mutable i = 0
    for r = row-1 to row+1 do
        for c = col - 1 to col + 1 do
            i <- (i <<< 1) ||| (getVirtualisedValue currentVirtualisedValue memory r c)
    lookupTable[i]

let inline iterate (lookupTable: int[]) (memory: int[,]) currentVirtualisedValue =
    let next = Array2D.init (Array2D.length1 memory + 2) (Array2D.length2 memory + 2) (fun row col -> lookupValue currentVirtualisedValue lookupTable memory (row - 1) (col - 1))
    next, nextVirtualisedValue lookupTable currentVirtualisedValue

let inline partOne(input: string list) =
    let lookupTable::_::initialImage = input

    let lookupTable = lookupTable.ToCharArray() |> Array.map (function | '#' -> 1 | _ -> 0)
    
    let rows = List.length initialImage

    let mutable initialMemory = Array2D.zeroCreate (rows) (rows)
    initialImage |> List.iteri (fun row -> String.iteri (fun col -> function | '#' -> initialMemory[row, col] <- 1; () | _ -> () ))

    let iterate = iterate lookupTable

    let mutable result = 0
    (initialMemory, 0) ||> iterate ||> iterate |> fst |> Array2D.iter (function | 1 -> incr &result | _ -> ())
    result


let inline partTwo(input: string list) =
    let lookupTable::_::initialImage = input

    let lookupTable = lookupTable.ToCharArray() |> Array.map (function | '#' -> 1 | _ -> 0)
    
    let rows = List.length initialImage

    let mutable initialMemory = Array2D.zeroCreate (rows) (rows)
    initialImage |> List.iteri (fun row -> String.iteri (fun col -> function | '#' -> initialMemory[row, col] <- 1; () | _ -> () ))

    let iterate = iterate lookupTable

    let rec loop iterations (memory, virtualisedValue) =
        match iterations with
        | 0 -> memory
        | _ -> loop (iterations - 1) (iterate memory virtualisedValue)

    let iteratedImage = loop 50 (initialMemory, 0)
    
    let mutable result = 0
    iteratedImage |> Array2D.iter (function | 1 -> incr &result | _ -> ())
    result

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadLinesList ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadLinesList ())

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
