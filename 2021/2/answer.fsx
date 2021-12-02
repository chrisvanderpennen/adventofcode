open System.IO

let lines = 
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Split(" ") |> function | [| dir; cnt |] -> (dir, int64 cnt))

let (h,d) =
    lines
    |> Array.fold
        (fun (h, d) -> 
            function
            | ("forward", n) -> (h + n, d)
            | ("down", n) -> (h, d + n)
            | ("up", n) -> (h, d - n))
        (0L, 0L)

printfn "%A" (h*d)

let (h2,d2,a) =
    lines
    |> Array.fold
        (fun (h, d, a) -> 
            function
            | ("forward", n) -> (h + n, d + (n * a), a)
            | ("down", n) -> (h, d, a + n)
            | ("up", n) -> (h, d, a - n))
        (0L, 0L, 0L)

printfn "%A" (h2*d2)