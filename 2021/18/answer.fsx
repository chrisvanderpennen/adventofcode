#if INTERACTIVE
#r "nuget:BenchmarkDotNet"
#r "nuget:FParsec"
#load "../util.fsx"
#endif

#if DEBUG
open Checked
#endif

open System
open BenchmarkDotNet.Attributes

[<StructuredFormatDisplay("{DisplayString}")>]
type Number =
| Value of int
| Node of Left: Number * Right: Number
with
    override this.ToString () =
        match this with
        | Value v -> sprintf "%i" v
        | Node (left, right) -> sprintf "[%s,%s]" (left.ToString()) (right.ToString())
    member this.DisplayString = this.ToString()

module Parser =
    open FParsec

    let num = pint32 |>> Value
    let node, nodeRef = createParserForwardedToRef ()
    let element = num <|> node
    let nodeContent = element .>> pchar ',' .>>. element |>> Node
    nodeRef.Value <- between (pchar '[') (pchar ']') nodeContent

    let parse = run node >> function
        | Success (number, _, _) -> number
        | Failure (err, _, _) -> failwith err

type ExplodeOperationType =
| Replace of Number
| Explode of Left: int * Right: int
| LiftExplosionLeft of Left: int * Right: Number
| LiftExplosionRight of Left: Number * Right: int

let split value =
    let struct(div, rem) = Math.DivRem(value, 2)
    Node (Value div, Value (div+rem))

let rec addLeft value = 
    function | Value v -> Value (v + value) | Node (left, right) -> Node (addLeft value left, right)

let rec addRight value = 
    function | Value v -> Value (v + value) | Node (left, right) -> Node (left, addRight value right)

let bubbleExplosionLeft right =
    function
    | Replace replacement -> 
        Replace (Node (replacement, right))

    | Explode (leftPart, rightPart) -> 
        LiftExplosionLeft (leftPart, Node (Value 0, addLeft rightPart right))

    | LiftExplosionLeft (leftPart, replacement) -> 
        LiftExplosionLeft (leftPart, Node(replacement, right))

    | LiftExplosionRight (replacement, rightPart) -> 
        Replace (Node (replacement, addLeft rightPart right))


let bubbleExplosionRight left =
    function
    | Replace replacement -> 
        Replace (Node (left, replacement))

    | Explode (leftPart, rightPart) -> 
        LiftExplosionRight (Node (addRight leftPart left, Value 0), rightPart)

    | LiftExplosionLeft (leftPart, replacement) -> 
        Replace (Node (addRight leftPart left, replacement))

    | LiftExplosionRight (replacement, rightPart) -> 
        LiftExplosionRight (Node(left, replacement), rightPart)

let rec walkExplosions depth =
    function
    | Value _ -> None
    | Node (Value left, Value right) when depth > 3 -> Some (Explode (left, right))
    | Node (left, right) ->
        walkExplosions (depth + 1) left 
        |> Option.map (bubbleExplosionLeft right)
        |> Option.orElseWith (fun () -> 
            walkExplosions (depth + 1) right 
            |> Option.map (bubbleExplosionRight left)
        )

let rec walkSplits =
    function
    | Value v when v > 9 -> Some (split v)
    | Value _ -> None
    | Node (left, right) ->
        walkSplits left
        |> Option.map (fun replace -> Node (replace, right))
        |> Option.orElseWith (fun () ->
            walkSplits right
            |> Option.map (fun replace -> Node (left, replace))
        )

let rec reduce number =
    let explodedNumber =
        walkExplosions 0 number
        |> Option.map (
            function
            | Replace replacement
            | LiftExplosionLeft (_, replacement)
            | LiftExplosionRight (replacement, _)
                -> replacement
            | Explode _ -> failwith "exploded at depth 0?"
        )
        |> Option.defaultValue number
    if explodedNumber <> number then reduce explodedNumber
    else 
        match walkSplits number with
        | Some replacement -> reduce replacement
        | None -> number

type Number with
    static member (+) (left, right) = reduce (Node (left, right))

let rec magnitude = 
    function
    | Value v -> v
    | Node (left, right) ->
        let left = 3 * magnitude left
        let right = 2 * magnitude right
        left + right

let rec allPairs list =
    match list with
    | []
    | [_] -> []
    | h::t ->
        List.map (fun t -> h,t) t @ allPairs t

let partOne(input: string list) =
    input
    |> List.map (Parser.parse)
    |> List.reduce (+)
    |> magnitude

let partTwo(input: string list) =
    input
    |> List.map (Parser.parse)
    |> allPairs
    |> List.collect (fun (l,r) -> [magnitude (l + r); magnitude (r + l)])
    |> List.max

type Answer () =
    inherit BaseAnswer()

    [<Benchmark>]
    member public this.PartOne () =
        partOne(this.loadLinesList ())

    [<Benchmark>]
    member public this.PartTwo () =
        partTwo( this.loadLinesList ())

let answer = Answer ()
answer.setup ()
printfn "%A" (answer.PartOne ())
printfn "%A" (answer.PartTwo ())

#if !INTERACTIVE
open BenchmarkDotNet.Running
BenchmarkRunner.Run<Answer>() |> ignore
#endif

// this makes static initialisers run
[<EntryPoint>]
let main _ =
    0
