#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes

let inline incr (i: int byref) 
    = i <- i + 1

let [<Literal>] Space = 0x20uy
let [<Literal>] Newline = 10uy
let [<Literal>] Pipe = 124uy

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.partOne () =
        let bytes = this.loadBinary ()

        let mutable c1 = 0
        let mutable c4 = 0
        let mutable c7 = 0
        let mutable c8 = 0

        let mutable i = 0
        while i < bytes.Length do
            if bytes[i] <> Pipe then
                incr &i
            else
                incr &i
                let mutable cc = 0
                while i < bytes.Length && bytes[i] <> Newline do
                    incr &i
                    if i = bytes.Length || bytes[i] = Space || bytes[i] = Newline then
                        match cc with
                        | 2 -> c1 <- c1 + 1
                        | 3 -> c7 <- c7 + 1
                        | 4 -> c4 <- c4 + 1
                        | 7 -> c8 <- c8 + 1
                        | _ -> ()
                        cc <- 0
                    else
                        cc <- cc + 1

        c1 + c4 + c7 + c8

    [<Benchmark>]
    member public this.partTwo () =
        let input = this.loadLinesList ()

        let split c (s: string) = s.Split [|c|] |> List.ofArray
        let trim (s: string) = s.Trim()

        let processLine (line: string) =
            let numberLists =
                line
                |> split '|'
                |> List.map (trim >> split ' ' >> List.map Set)
            
            let numberGroups = 
                numberLists
                |> List.collect id
                |> List.distinct
                |> List.groupBy Set.count
                |> Map
            
            let [n1] = numberGroups[2]
            let [n4] = numberGroups[4]
            let [n7] = numberGroups[3]
            let [n8] = numberGroups[7]

            let cf = n1
            let a = Set.difference n7 n1
            let bd = Set.difference n4 n1
            let eg = 
                Set.difference n8 <| Set.union n4 a
                
            let [n6], rest6 = List.partition (Set.isSubset (bd |> Set.union eg |> Set.union a)) numberGroups[6]
            let [n9], [n0] = List.partition (Set.isSubset bd) rest6

            let g = Set.intersect eg n9
            let d = Set.difference bd n0
            let f = Set.intersect cf n6
            let c = Set.difference cf f
            
            let n2 = Set.unionMany [a; c; d; eg]
            let n3 = Set.unionMany [n1; a; d; g]
            let n5 = Set.unionMany [a; bd; f; g]
            
            let costs = Map [
                n1, 1
                n2, 2
                n3, 3
                n4, 4
                n5, 5
                n6, 6
                n7, 7
                n8, 8
                n9, 9
                n0, 0
            ]

            let [_;outputs] = numberLists
            outputs |> List.mapi (fun i e -> costs[e] * (pown 10 (3 - i))) |> List.sum

        List.sumBy processLine input
        
let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
