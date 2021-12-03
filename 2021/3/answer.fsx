open System
open System.IO

let lines = 
    File.ReadLines "input.txt"
    |> Seq.map (fun s -> s.ToCharArray())
    |> List.ofSeq
    
let getMostCommonBit i (lst: char[] list) =
    let res =
        lst
        |> List.map (Array.item i)
        |> List.countBy id
        |> List.maxBy (fun (a,b) -> b, match a with | '0' -> 0 | '1' -> 1)
    fst res

let swapBit = function | '0' -> '1' | '1' -> '0'

let mostCommon = 
    [0..11]
    |> List.map (fun i -> getMostCommonBit i lines)
    |> Array.ofList

let mostCommonString = String mostCommon
let mostCommonNumber = Convert.ToUInt32(mostCommonString, 2)

let leastCommon = Array.map swapBit mostCommon
let leastCommonString = String leastCommon
let leastCommonNumber = Convert.ToUInt32(leastCommonString, 2)

printfn "%A %A %A" mostCommonNumber leastCommonNumber (mostCommonNumber * leastCommonNumber)

let [o2], [co2] =
    [0..11]
    |> List.fold 
        (fun (o2, co2) i ->
            (match o2 with | [h] -> o2 | _ -> List.filter (Array.item i >> (=) (getMostCommonBit i o2)) o2), (match co2 with | [h] -> co2 | _ -> List.filter (Array.item i >> (=) (getMostCommonBit i co2 |> swapBit)) co2)
        ) (lines, lines)

let o2number = Convert.ToUInt32((String o2), 2)
let co2number = Convert.ToUInt32((String co2), 2)

printfn "%A %A %A" o2number co2number (o2number * co2number)