
#r "nuget:FSharp.Text.RegexProvider"
open System.IO
open System.Diagnostics
open FSharp.Text.RegexProvider

let time (sw: Stopwatch) =
    printfn "%A" sw.Elapsed
    sw.Restart()

let sw = Stopwatch.StartNew()

let lines = 
    File.ReadLines "input.txt"

type InputState = {
    Current: Map<string, string>
    Scanned: List<Map<string, string>>
}

let scanInput (state: InputState) (line: string) =
    match line with
    | "" -> { Scanned = state.Current :: state.Scanned; Current = Map.empty }
    | _ ->
        let hunks = line.Split(' ');
        { state with Current = hunks |> Array.fold (fun map hunk -> let [|a; b|] = hunk.Split(':') in Map.add a b map) state.Current }

let parsed = Seq.fold scanInput {Current = Map.empty; Scanned = List.empty} lines

let passportList = parsed.Current :: parsed.Scanned

let expectedFields = [
    "byr"
    "iyr"
    "eyr"
    "hgt"
    "hcl"
    "ecl"
    "pid"
]

let validity: List<Map<string, string> -> bool> = List.map (Map.containsKey) expectedFields

module List =
    let apply lst a = List.map (fun b -> b a) lst

let validity1 = List.apply validity >> (List.reduce (&&))

let validPassports = List.filter validity1 passportList

printfn "%A" validPassports.Length

let (>=<) x (min, max) =
    (x >= min) && (x <= max)

type HeightRegex = Regex< @"^(?<Height>[0-9]+)(?<Unit>(cm|in))$" >
let re = HeightRegex()

let eyeColours = Set [
    "amb"
    "blu"
    "brn"
    "gry"
    "grn"
    "hzl"
    "oth"
]

let fieldValidity k v =
    match k with
    | "byr" -> Regex.IsMatch(v, "^\\d{4}$") && (int v) >=< (1920, 2002)
    | "iyr" -> Regex.IsMatch(v, "^\\d{4}$") && (int v) >=< (2010, 2020)
    | "eyr" -> Regex.IsMatch(v, "^\\d{4}$") && (int v) >=< (2020, 2030)
    | "hgt" ->
        match re.TryTypedMatch v with
            | None -> false
            | Some res ->
                match res.Unit.Value with
                | "cm" -> (int res.Height.Value) >=< (150, 193)
                | "in" -> (int res.Height.Value) >=< (59, 76)
                | _ -> false
    | "hcl" -> Regex.IsMatch (v, "^#[0-9a-f]{6}$")
    | "ecl" -> Set.contains v eyeColours
    | "pid" -> Regex.IsMatch (v, "^[0-9]{9}$")
    | "cid" -> true
    | _ -> false

let validity2 = Map.fold (fun state k v -> fieldValidity k v && state) true

let validPassports2 = List.filter validity2 validPassports

printfn "%A" validPassports2.Length
