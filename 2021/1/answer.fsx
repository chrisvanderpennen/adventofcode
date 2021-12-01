open System.IO

let lines = File.ReadAllLines "input.txt"

printfn "%A" (lines |> Seq.map int |> Seq.pairwise |> Seq.filter (fun elem -> snd elem > fst elem) |> Seq.length)

printfn "%A" (lines |> Seq.map int |> Seq.windowed 3 |> Seq.map (Array.reduce (+)) |> Seq.pairwise |> Seq.filter (fun elem -> snd elem > fst elem) |> Seq.length)