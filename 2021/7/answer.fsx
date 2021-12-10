#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#load "../util.fsx"
#endif

open System
open BenchmarkDotNet.Attributes
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open System.Runtime.InteropServices

let part2DistanceSumVectorised (pos: int16, crabs: Span<int16> inref) =
    let mutable tail: Span<int16> = Span<int16>()
    let crabsVector = vectorise128 crabs &tail
    let mutable result = Span<int>()
    stackalloc(&result, crabs.Length)
    let mutable resultTail: Span<int> = Span<int>()
    let resultVector = vectorise result &resultTail

    for i = 0 to crabsVector.Length - 1 do
        let dst = Avx2.ConvertToVector256Int32(Avx2.Subtract(crabsVector[i], Vector128.Create pos) |> Avx2.Abs)
        resultVector[i] <- Avx2.ShiftRightLogical(Avx2.MultiplyLow(dst, Avx2.Add(dst, Vector256.Create 1)), 1uy)

    for i = 0 to tail.Length - 1 do
        let dst = int (abs(pos - tail[i]))
        resultTail[i] <- dst * (dst + 1) / 2

    for i = 1 to resultVector.Length - 1 do
        resultVector[0] <- Avx2.Add(resultVector[0], resultVector[i])
    
    let mutable res = 0
    for i = 0 to Vector256<int>.Count - 1 do
        res <- res + result[i]
    
    for i = 0 to resultTail.Length - 1 do
        res <- res + resultTail[i]
    
    res

let part2DistanceSumVectorisedi32 (pos: int, crabs: Span<int> inref) =
    let mutable tail: Span<int> = Span<int>()
    let crabsVector = vectorise crabs &tail
    let mutable result = Span<uint>()
    stackalloc(&result, crabs.Length)
    let mutable resultTail: Span<uint> = Span<uint>()
    let resultVector = vectorise result &resultTail

    for i = 0 to crabsVector.Length - 1 do
        let dst = Avx2.Subtract(crabsVector[i], Vector256.Create pos) |> Avx2.Abs
        resultVector[i] <- Avx2.ShiftRightLogical(Avx2.MultiplyLow(dst, Avx2.Add(dst, Vector256.Create 1u)), 1uy)

    for i = 0 to tail.Length - 1 do
        let dst = uint (abs(pos - tail[i]))
        resultTail[i] <- dst * (dst + 1u) / 2u

    for i = 1 to resultVector.Length - 1 do
        resultVector[0] <- Avx2.Add(resultVector[0], resultVector[i])
    
    let mutable res = 0u
    for i = 0 to Vector256<int>.Count - 1 do
        res <- res + result[i]
    
    for i = 0 to resultTail.Length - 1 do
        res <- res + resultTail[i]
    
    res

let rec gradientDescentVectorised(dMin, pMin, dMax, pMax, span: Span<int16> inref) =
    match abs(pMax - pMin) with
    | 0s
    | 1s -> 
        if dMin < dMax then
            struct(dMin, pMin)
        else 
            struct(dMax, pMax)
    | range ->
        let p1 = (min pMin pMax) + (range / 2s)
        let d1 = part2DistanceSumVectorised(p1, &span)
        if dMin < dMax then gradientDescentVectorised(dMin, pMin, d1, p1, &span) else gradientDescentVectorised(dMax, pMax, d1, p1, &span)

type Answer () =
    inherit BaseAnswer()

    //[<Benchmark>]
    member public this.countCommas () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

    // [<Benchmark>]
    member public this.partOne_FirstAttempt () =
        let input = this.loadBinary ()
        let arr = Array.zeroCreate 1000

        let mutable i = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        Array.sortInPlace arr
        let median = arr[arr.Length / 2]
        let diff = Array.sumBy (fun i -> (median - i) |> abs) arr
        diff

    [<Benchmark>]
    member public this.partOne_Median () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let arr = Array.zeroCreate (commas + 1)

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        Array.sortInPlace arr
        let median = arr[arr.Length / 2]
        let diff = Array.sumBy (fun i -> (median - i) |> abs) arr
        diff

    [<Benchmark>]
    member public this.partOne () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let arr = Array.zeroCreate (commas + 1)

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        let inline distance a b = abs(b - a)

        let rec gradientDescent dMin pMin dMax pMax =
            match abs(pMax - pMin) with
            | 0
            | 1 -> if dMin < dMax then dMin, pMin else dMax, pMax
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (distance p1) arr
                if dMin < dMax then gradientDescent dMin pMin d1 p1 else gradientDescent dMax pMax d1 p1

        let distance, fm = gradientDescent (Array.sumBy (distance minV) arr) minV (Array.sumBy (distance maxV) arr) maxV
        fm, distance

    [<Benchmark>]
    member public this.partTwo () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let arr = Array.zeroCreate (commas + 1)

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10 + int32(input[p] &&& 0x0fuy)

        let inline distance a b =
            let dst = abs(b - a)
            dst * (dst + 1) / 2

        let rec gradientDescent dMin pMin dMax pMax =
            match abs(pMax - pMin) with
            | 0
            | 1 -> 
                if dMin < dMax then
                    struct(dMin, pMin)
                else 
                    struct(dMax, pMax)
            | range ->
                let p1 = (min pMin pMax) + (range / 2)
                let d1 = Array.sumBy (distance p1) arr
                if dMin < dMax then gradientDescent dMin pMin d1 p1 else gradientDescent dMax pMax d1 p1

        let struct(distance, fm) = gradientDescent (Array.sumBy (distance minV) arr) minV (Array.sumBy (distance maxV) arr) maxV
        fm, distance, (double (Array.sum arr) / double arr.Length)

    [<Benchmark>]
    member public this.partTwoVectorSum () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let mutable arr = Span<int16>()
        stackalloc(&arr, commas + 1)

        let mutable i = 0
        let mutable minV = Int16.MaxValue
        let mutable maxV = 0s
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV arr[i]
                maxV <- max maxV arr[i]
                i <- i + 1
            else
                arr[i] <- arr[i] * 10s + int16(input[p] &&& 0x0fuy)
        
        let inline distance a b =
            let dst = abs(b - a)
            dst * (dst + 1s) / 2s

        let struct(distance, fm) = gradientDescentVectorised( (part2DistanceSumVectorised(minV, &arr)), minV, (part2DistanceSumVectorised(maxV, &arr)), maxV, &arr)
        fm, distance

    [<Benchmark>]
    member public this.partTwoNumeric () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let mutable ispan = Span<int16>()
        stackalloc(&ispan, commas + 1)
        let mutable fspan = Span<single>()
        stackalloc(&fspan, commas + 1)

        let mutable i = 0
        let mutable minV = Int16.MaxValue
        let mutable maxV = 0s
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV ispan[i]
                maxV <- max maxV ispan[i]
                fspan[i] <- single ispan[i]
                i <- i + 1
            else
                ispan[i] <- ispan[i] * 10s + int16(input[p] &&& 0x0fuy)
        
        let fcrabcount = single (commas + 1)
        let mutable sum = 0
        for i = 0 to ispan.Length - 1 do
            sum <- sum + int ispan[i]
        let mean = single sum / fcrabcount

        let mutable countBelowMean = 0
        for i = 0 to fspan.Length - 1 do
            if fspan[i] < mean then
                countBelowMean <- countBelowMean + 1

        let inline distance a b =
            let dst = abs(b - a)
            dst * (dst + 1) / 2

        let meanCorrection = (fcrabcount - ((single countBelowMean) * 2.0f))/(2.0f * fcrabcount)

        let fmedian = mean + meanCorrection
        let median = int16 (round fmedian)

        let distance = part2DistanceSumVectorised(median, &ispan) // Array.sumBy (distance median) arr
        median, distance


    [<Benchmark>]
    member public this.partTwoNumericVectorised () =
        let input = this.loadBinary ()
        let mutable tail: Span<byte> = Span<byte>()
        let iv = vectorise input &tail
        let mutable counts = Unchecked.defaultof<Vector256<byte>>
        for i = 0 to iv.Length - 1 do
            counts <- Avx2.Subtract(counts, Avx2.CompareEqual(iv[i], Vector256.Create (byte ',')))
        
        let result = Avx2.SumAbsoluteDifferences(counts, Vector256.Zero).As<uint16, int32>()
        let mutable tailCount = 0
        for i = 0 to tail.Length - 1 do
            if tail[i] = byte ',' then tailCount <- tailCount + 1
        let commas = result.GetElement 0 + result.GetElement 2 + result.GetElement 4 + result.GetElement 6 + tailCount

        let mutable ispan = Span<int>()
        stackalloc(&ispan, commas + 1)
        let mutable fspan = Span<single>()
        stackalloc(&fspan, commas + 1)

        let mutable i = 0
        let mutable minV = Int32.MaxValue
        let mutable maxV = 0
        for p = 0 to input.Length - 1 do
            if input[p] = (byte ',')
            then
                minV <- min minV ispan[i]
                maxV <- max maxV ispan[i]
                fspan[i] <- single ispan[i]
                i <- i + 1
            else
                ispan[i] <- ispan[i] * 10 + int(input[p] &&& 0x0fuy)
        
        let mutable ftail = Span<single>()
        let fv = vectorise (fspan) &ftail
        let mutable itail = Span<int>()
        let iiv = vectorise (ispan) &itail

        let fcrabcount = single fspan.Length

        let mutable sum = Vector256<int>.Zero
        for i = 0 to iiv.Length - 1 do
            sum <- Avx2.Add(sum, iiv[i])
        
        let sumspan = MemoryMarshal.Cast<Vector256<int>, int>(MemoryMarshal.CreateSpan(&sum, 1))

        let mutable isum = 0
        for i = 0 to sumspan.Length - 1 do
            isum <- isum + sumspan[i]
        for i = 0 to itail.Length - 1 do
            isum <- isum + itail[i]
        let mean = single isum / fcrabcount

        let mutable countBelowMean = 0u
        let mutable countVector = Vector256<uint>.Zero
        for i = 0 to fv.Length - 1 do
            countVector <- Avx2.Subtract(countVector, Avx2.CompareLessThan(fv[i], Vector256.Create mean).As<single, uint32>())

        let countspan = MemoryMarshal.Cast<Vector256<uint32>, uint32>(MemoryMarshal.CreateSpan(&countVector, 1))

        for i = 0 to countspan.Length - 1 do
            countBelowMean <- countBelowMean + countspan[i]
        for i = 0 to ftail.Length - 1 do
            if ftail[i] < mean then
                countBelowMean <- countBelowMean + 1u

        let meanCorrection = 0.5f - (single countBelowMean / fcrabcount)
        let fmedian = mean + meanCorrection
        let median = int (round fmedian)

        let distance = part2DistanceSumVectorisedi32(median, &ispan) // Array.sumBy (distance median) arr
        median, distance

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.countCommas ())
printfn "%A" (answer.partOne ())
printfn "%A" (answer.partTwo ())
printfn "%A" (answer.partTwoVectorSum ())
printfn "%A" (answer.partTwoNumeric ())
printfn "%A" (answer.partTwoNumericVectorised ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif
