open System
open System.IO

let lines = 
    File.ReadAllLines "input.txt"
    |> Array.map Array.ofSeq
    
let transpose = Array.transpose lines

let swapBit = function | '0' -> '1' | '1' -> '0'

let mostCommon = transpose |> Array.map (Array.countBy id >> Array.maxBy snd >> fst)
let mostCommonString = String mostCommon
let mostCommonNumber = Convert.ToUInt32(mostCommonString, 2)

let leastCommon = Array.map swapBit mostCommon
let leastCommonString = String leastCommon
let leastCommonNumber = Convert.ToUInt32(leastCommonString, 2)

printfn "%A %A %A" mostCommonNumber leastCommonNumber (mostCommonNumber * leastCommonNumber)

let initList = List.ofArray lines

let getCO2Bit i (lst: char[] list) =
    let res =
        lst
        |> List.map (Array.item i)
        |> List.countBy id
        |> List.minBy (fun (a,b) -> b, match a with | '0' -> 0 | '1' -> 1)
    fst res

let [o2], [co2] =
    [0..11]
    |> List.fold 
        (fun (o2, co2) i ->
            (match o2 with | [h] -> o2 | _ -> List.filter (Array.item i >> (=) (getCO2Bit i o2 |> swapBit)) o2), (match co2 with | [h] -> co2 | _ -> List.filter (Array.item i >> (=) (getCO2Bit i co2)) co2)
        ) (initList, initList)

printfn "%A" o2
printfn "%A" co2

let o2number = Convert.ToUInt32((String o2), 2)
let co2number = Convert.ToUInt32((String co2), 2)

printfn "%A %A %A" o2number co2number (o2number * co2number)