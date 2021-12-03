open System.IO
open System.Diagnostics

let time (sw: Stopwatch) =
    printfn "%A" sw.Elapsed
    sw.Restart()

let sw = Stopwatch.StartNew()

let rec combinations count list = 
    match count, list with
    | _, [] -> []
    | 1, _ -> List.map (fun x -> [x]) list
    | k, (x::xs) -> List.map (fun l -> x::l) (combinations (k-1) xs) @ combinations k xs

let ints =
    File.ReadLines """input.txt"""
    |> Seq.map int
    |> List.ofSeq

time sw

combinations 2 ints
    |> Seq.filter ((List.reduce (+)) >> ((=) 2020))
    |> Seq.map (List.reduce (*))
    |> printfn "%A"

time sw

combinations 3 ints
    |> Seq.filter ((List.reduce (+)) >> ((=) 2020))
    |> Seq.map (List.reduce (*))
    |> printfn "%A"

time sw
