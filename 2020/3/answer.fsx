open System.IO
open System.Diagnostics

let time (sw: Stopwatch) =
    printfn "%A" sw.Elapsed
    sw.Restart()

let sw = Stopwatch.StartNew()

let readLine =
    Seq.map (function | '.' -> 0u | '#' -> 1u) >> Array.ofSeq

let lines = 
    File.ReadLines "input.txt"
    |> Seq.map readLine
    |> List.ofSeq

time sw

let countTrees xstride (lines: uint[] list) =
    lines
    |> List.fold (fun (x, s) arr -> (x + xstride) % arr.Length, s + arr.[x]) (0, 0u)
    |> snd

lines |> countTrees 3 |> printfn "%A"

time sw


let slopes = [
    1, 1
    3, 1
    5, 1
    7, 1
    1, 2
]

let linesMatching stride =
    match stride with
    | 1 -> id
    | n -> List.chunkBySize n >> List.map List.head

slopes
    |> List.map (
        fun (xstride, stride) ->
            lines
            |> linesMatching stride
            |> countTrees xstride
    )
    |> List.map (uint)
    |> List.reduce (*)
    |> printfn "%A"

time sw
