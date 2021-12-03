#r "nuget:FSharp.Text.RegexProvider"

open System.IO
open FSharp.Text.RegexProvider

type PolicyRegex = Regex< @"(?<Min>^\d+)-(?<Max>\d+)\s+(?<Character>\w):\s+(?<Password>\w+)" >
let re = PolicyRegex()

let strings = 
    File.ReadLines "input.txt"
    |> Seq.choose re.TryTypedMatch

let meetsPolicy min max char password =
    let len = 
        Seq.filter ((=) char) password
        |> Seq.length
    len >= min && len <= max

let meetsPolicy2 a b char (password: string) =
    let pos1 = password.[a-1] = char
    let pos2 = password.[b-1] = char
    pos1 <> pos2

strings
    |> Seq.filter (fun re -> meetsPolicy (int re.Min.Value) (int re.Max.Value) (re.Character.Value.[0]) re.Password.Value)
    |> Seq.length
    |> printfn "%A"

strings
    |> Seq.filter (fun re -> meetsPolicy2 (int re.Min.Value) (int re.Max.Value) (re.Character.Value.[0]) re.Password.Value)
    |> Seq.length
    |> printfn "%A"
