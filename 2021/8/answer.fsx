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
    
    let digits = Map [
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
    outputs |> List.mapi (fun i e -> digits[e] * (pown 10 (3 - i))) |> List.sum

let [<Literal>] A = 0b0000_0001uy
let [<Literal>] B = 0b0000_0010uy
let [<Literal>] C = 0b0000_0100uy
let [<Literal>] D = 0b0000_1000uy
let [<Literal>] E = 0b0001_0000uy
let [<Literal>] F = 0b0010_0000uy
let [<Literal>] G = 0b0100_0000uy

let [<Literal>] Achar = 97uy
let [<Literal>] Bchar = 98uy
let [<Literal>] Cchar = 99uy
let [<Literal>] Dchar = 100uy
let [<Literal>] Echar = 101uy
let [<Literal>] Fchar = 102uy
let [<Literal>] Gchar = 103uy

let part2Binary (input: Span<byte>) =
    
    let bitmap = [|A;B;C;D;E;F;G|]
    let mutable i = 0
    let mutable res = 0
    
    while i < input.Length do

        let digits = Array.zeroCreate<byte> 10
        let fives = Array.zeroCreate<byte> 3
        let sixes = Array.zeroCreate<byte> 3
        // digits
        let mutable len = 0
        let mutable num = 0uy
        let mutable nFives = 0
        let mutable nSixes = 0
        while input[i] <> Pipe do
            match input[i] with
            | Space ->
                match len with
                | 2 -> digits[1] <- num
                | 3 -> digits[7] <- num
                | 4 -> digits[4] <- num
                | 7 -> digits[8] <- num
                | 5 ->
                    fives[nFives] <- num
                    incr &nFives
                | 6 -> 
                    sixes[nSixes] <- num
                    incr &nSixes
                len <- 0
                num <- 0uy
            | _ -> 
                incr &len
                num <- num ||| (bitmap[int( input[i] - Achar )])
            incr &i
        
        // move past the pipe
        incr &i

        let outputs = Array.zeroCreate 4
        let mutable count = 0
        // outputs
        while i < input.Length && input[i] <> Newline do
            let mutable num = 0uy
            if input[i] = Space then incr &i
            while i < input.Length && input[i] <> Space && input[i] <> Newline do
                num <- num ||| (bitmap[int( input[i] - Achar )])
                incr &i
            outputs[count] <- num
            incr &count
        
        // move past the newline
        incr &i

        let cf = digits[1]
        let a = digits[7] &&& ~~~ cf
        let bd = digits[4] &&& ~~~ cf
        let eg = digits[8] &&& ~~~(digits[4] ||| a)
        let i6 = Array.findIndexInline (fun digit -> digit &&& (bd ||| eg ||| a) = (bd ||| eg ||| a)) sixes
        digits[6] <- sixes[i6]
        sixes[i6] <- 0uy
        let i9 = Array.findIndexInline (fun digit -> digit <> digits[6] && digit &&& bd = bd) sixes
        digits[9] <- sixes[i9]
        sixes[i9] <- 0uy

        digits[0] <- Array.find (fun digit -> digit <> 0uy) sixes

        let g = eg &&& digits[9]
        let d = bd &&& ~~~digits[0]
        let f = cf &&& digits[6]
        let c = cf &&& ~~~f

        digits[2] <- a ||| c ||| d ||| eg
        digits[3] <- cf ||| a ||| d ||| g
        digits[5] <- a ||| bd ||| f ||| g

        // Array.iter (printf "%B ") digits
        // printf "|"
        // Array.iter (printf " %B") outputs
        // printfn ""

        res <- res + (outputs |> Array.mapi (fun i e -> (Array.IndexOf(digits, e)) * (pown 10 (3 - i))) |> Array.sum)

    res


let part2Binary2 (input: Span<byte>) =
    
    let bitmap = [|A;B;C;D;E;F;G|]
    let mutable i = 0
    let mutable res = 0
    
    while i < input.Length do

        let digits = Array.zeroCreate<byte> 10
        let fives = Array.zeroCreate<byte> 3
        let sixes = Array.zeroCreate<byte> 3
        // digits
        let mutable len = 0
        let mutable num = 0uy
        let mutable nFives = 0
        let mutable nSixes = 0
        while input[i] <> Pipe do
            match input[i] with
            | Space ->
                match len with
                | 2 -> digits[1] <- num
                | 3 -> digits[7] <- num
                | 4 -> digits[4] <- num
                | 7 -> digits[8] <- num
                | 5 ->
                    fives[nFives] <- num
                    incr &nFives
                | 6 -> 
                    sixes[nSixes] <- num
                    incr &nSixes
                len <- 0
                num <- 0uy
            | _ -> 
                incr &len
                num <- num ||| (bitmap[int( input[i] - Achar )])
            incr &i
        
        // move past the pipe
        incr &i

        let outputs = Array.zeroCreate 4
        let mutable count = 0
        // outputs
        while i < input.Length && input[i] <> Newline do
            let mutable num = 0uy
            if input[i] = Space then incr &i
            while i < input.Length && input[i] <> Space && input[i] <> Newline do
                num <- num ||| (bitmap[int( input[i] - Achar )])
                incr &i
            outputs[count] <- num
            incr &count
        
        // move past the newline
        incr &i

        digits[9] <- sixes[sixes |> Array.findIndexInline (fun digit -> digits[4] &&& digit = digits[4])]
        digits[6] <- sixes[sixes |> Array.findIndexInline (fun digit -> digits[1] &&& digit <> digits[1])]
        digits[0] <- sixes[sixes |> Array.findIndexInline (fun digit -> (digit ||| digits[1]) &&& digits[4] <> digits[4])]

        digits[3] <- fives[fives |> Array.findIndexInline (fun digit -> digit &&& digits[1] = digits[1])]
        digits[2] <- fives[fives |> Array.findIndexInline (fun digit -> (digit ||| digits[4]) = digits[8])]
        digits[5] <- fives[fives |> Array.findIndexInline (fun digit -> (digit ||| digits[1]) &&& digits[4] = digits[4])]

        res <- res + (outputs |> Array.mapi (fun i e -> (Array.IndexOf(digits, e)) * (pown 10 (3 - i))) |> Array.sum)

    res

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

        List.sumBy processLine input
        
    [<Benchmark>]
    member public this.partTwoBinary () =
        let input = this.loadBinary ()
        part2Binary input

    [<Benchmark>]
    member public this.partTwoBinary2 () =
        let input = this.loadBinary ()
        part2Binary2 input
        
        
let answer = Answer ()
answer.setup ()
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())
printfn "%A" (answer.partTwoBinary ())
printfn "%A" (answer.partTwoBinary2 ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
