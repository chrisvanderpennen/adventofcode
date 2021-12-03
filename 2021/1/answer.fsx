open System.IO

let lines = 
    File.ReadAllLines "input.txt"
    |> Array.map int

printfn "%A" (lines |> Seq.pairwise |> Seq.filter ((<||) (<)) |> Seq.length)

printfn "%A" (lines |> Seq.windowed 3 |> Seq.map (Array.reduce (+)) |> Seq.pairwise |> Seq.filter ((<||) (<)) |> Seq.length)
