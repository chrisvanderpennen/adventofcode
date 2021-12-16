#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes
let inline readBitAt (data: byte[]) offset =
    (match offset % 8 with
    | 0 -> data[offset / 8] >>> 7
    | shift -> 
        ((data[offset / 8]) >>> (7 - shift)) &&& 0b0000_0001uy) = 1uy

let inline readBitsAt (data: byte[]) offset length =
    match offset % 8 with
    | 0 -> data[offset / 8] >>> (8 - length)
    | shift when shift + length <= 8 -> 
        (data[offset / 8] <<< shift) >>> (8 - length)
    | shift -> 
        ((data[offset / 8] <<< (shift)) + (data[offset / 8 + 1] >>> (8 - shift))) >>> (8 - length)

let rec consumeLiteral data offset =
    let hunk = readBitsAt data offset 5
    // printf "<   >"
    if hunk > 0b0000_1111uy then
        consumeLiteral data (offset + 5)
    else
        offset + 5

let rec readLiteral data offset =
    let hunk = readBitsAt data offset 5
    if hunk > 0b0000_1111uy then
        let struct(tail, endOffset, hunks) = readLiteral data (offset + 5)
        struct(((uint64 (hunk &&& 0b0000_1111uy)) <<< (hunks * 4)) ||| tail, endOffset, hunks + 1)
    else
        struct(uint64 (hunk &&& 0b0000_1111uy), offset + 5, 1)

let rec scanPacketVersionNumbers data offset =
    let localVersion = int <| readBitsAt data offset 3
    // printf "VVV"
    let packetType = readBitsAt data (offset + 3) 3
    // printf "III"
    match packetType with
    | 4uy -> // literal
        let endoffset = consumeLiteral data (offset + 6)
        struct(localVersion, endoffset)
    | _ -> // operator
        let lengthType = readBitAt data (offset + 6)
        // printf "T"
        let offset = offset + 7
        match lengthType with
        | false -> // bit count
            let length = 
                ((int <| readBitsAt data offset 8) <<< 7)
                + (int <| readBitsAt data (offset + 8) 7)
            let mutable offset = offset + 15
            // printf "%s" (String.replicate 15 "L")
            let endOffset = offset + length
            let mutable nestedVersions = 0
            while offset < endOffset do
                let struct(innerVersion, innerOffset) = scanPacketVersionNumbers data offset
                offset <- innerOffset
                nestedVersions <- nestedVersions + innerVersion
            struct(nestedVersions + localVersion, endOffset)
                
        | true -> // packet count
            let packetCount = 
                ((int <| readBitsAt data offset 8) <<< 3)
                + (int <| readBitsAt data (offset + 8) 3)
            // printf "%s" (String.replicate 11 "C")
            let mutable offset = offset + 11
            let mutable nestedVersions = 0
            for _ = 1 to packetCount do
                let struct(innerVersion, innerOffset) = scanPacketVersionNumbers data offset
                offset <- innerOffset
                nestedVersions <- nestedVersions + innerVersion
            
            struct(nestedVersions + localVersion, offset)

let rec inline evaluateListOperator data offset ([<InlineIfLambda>]reducer) =
    let lengthType = readBitAt data offset
    let offset = offset + 1
    match lengthType with
    | false -> // bit count
        let length = 
            ((int <| readBitsAt data offset 8) <<< 7)
            + (int <| readBitsAt data (offset + 8) 7)
        let mutable offset = offset + 15
        let mutable acc = 0uL
        let endOffset = offset + length
        
        let struct(value, nextOffset) = evaluateBITSpacket data offset
        offset <- nextOffset
        acc <- value

        while offset < endOffset do
            // printf ", "
            let struct(value, nextOffset) = evaluateBITSpacket data offset
            offset <- nextOffset
            acc <- reducer acc value
        
        // printf "]=%i" acc
        struct(acc, offset)

    | true -> // packet count
        let packetCount = 
            ((int <| readBitsAt data offset 8) <<< 3)
            + (int <| readBitsAt data (offset + 8) 3)
        let mutable offset = offset + 11
        let mutable acc = 0uL

        let struct(value, nextOffset) = evaluateBITSpacket data offset
        offset <- nextOffset
        acc <- value

        if packetCount = 1 then 
            // printf "]=%i" acc
            struct(acc, offset)
        else
            for _ = 2 to packetCount do
                // printf ", "
                let struct(value, nextOffset) = evaluateBITSpacket data offset
                offset <- nextOffset
                acc <- reducer acc value
            // printf "]=%i" acc
            struct(acc, offset)

and inline evaluatePairOperator data offset ([<InlineIfLambda>]reducer) =
    let lengthType = readBitAt data offset
    let offset = offset + 1
    match lengthType with
    | false -> // bit count
        let offset = offset + 15
        let struct(a, offset) = evaluateBITSpacket data offset
        // printf ", "
        let struct(b, offset) = evaluateBITSpacket data offset
        // printf "]"
        let res = reducer a b
        // printf "=%i" res
        struct(res, offset)

    | true -> // packet count
        let offset = offset + 11
        let struct(a, offset) = evaluateBITSpacket data offset
        // printf ", "
        let struct(b, offset) = evaluateBITSpacket data offset
        // printf "]"
        
        let res = reducer a b
        // printf "=%i" res
        struct(res, offset)

and evaluateBITSpacket data offset =
    let packetType = readBitsAt data (offset + 3) 3
    let offset = offset + 6
    match packetType with
    | 4uy -> // literal
        let struct(literal, endoffset, _) = readLiteral data offset
        // printf "%i" literal
        struct(literal, endoffset)
    | 0uy -> // sum
        // printf "sum["
        evaluateListOperator data offset (fun a b -> a + b)
    | 1uy -> // product
        // printf "product["
        evaluateListOperator data offset (fun a b -> a * b)
    | 2uy -> // min
        // printf "min["
        evaluateListOperator data offset (fun a b -> min a b)
    | 3uy -> // max
        // printf "max["
        evaluateListOperator data offset (fun a b -> max a b)
    | 5uy -> // greater than
        // printf "gt["
        evaluatePairOperator data offset (fun a b -> if a > b then 1uL else 0uL)
    | 6uy -> // less than
        // printf "lt["
        evaluatePairOperator data offset (fun a b -> if a < b then 1uL else 0uL)
    | 7uy -> // equal
        // printf "eq["
        evaluatePairOperator data offset (fun a b -> if a = b then 1uL else 0uL)

let inline partOne(input: ReadOnlySpan<char>) =
    let data = Convert.FromHexString(input)
    // Array.iter (printf "%08B") data
    // printfn ""
    let res = scanPacketVersionNumbers data 0
    // printfn ""
    res


let inline partTwo(input: ReadOnlySpan<char>) =
    let data = Convert.FromHexString(input)
    // Array.iter (printf "%08B") data
    // printfn ""
    let res = evaluateBITSpacket data 0
    // printfn ""
    res

// let inline parse (input: ReadOnlySpan<char>) =
//     let data = Convert.FromHexString(input)
//     data

// let chartostring = 
//     function
//     | '0' -> "0000"
//     | '1' -> "0001"
//     | '2' -> "0010"
//     | '3' -> "0011"
//     | '4' -> "0100"
//     | '5' -> "0101"
//     | '6' -> "0110"
//     | '7' -> "0111"
//     | '8' -> "1000"
//     | '9' -> "1001"
//     | 'A' -> "1010"
//     | 'B' -> "1011"
//     | 'C' -> "1100"
//     | 'D' -> "1101"
//     | 'E' -> "1110"
//     | 'F' -> "1111"

// let inline parseWiden (input: ReadOnlySpan<char>) =
//     let sb = StringBuilder(input.Length * 4)
//     for c in input do
//         sb.Append (chartostring c)
//     sb.ToString()

type Answer () =
    inherit BaseAnswer()

    // [<Benchmark>]
    // member public this.parse () =
    //     parse(this.loadChars ())

    // [<Benchmark>]
    // member public this.parseWiden () =
    //     parseWiden(this.loadChars ())

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadChars ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadChars ())

let answer = Answer ()
answer.setup ()
// printfn "%A" (answer.PartOne ())
// printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
