open System.IO

let lines = File.ReadAllLines("Day5/input.txt")

type MappingInput = {
    SourceRangeStart: float
    DestinationRangeStart: float
    RangeLength: float
}

type InputSeed = InputSeed of float

let rawSeedLine = Array.head lines
let seeds = ((rawSeedLine.Split(":").[1]).Trim()).Split(" ") |> Array.map (fun x -> InputSeed (float x))
let getMapping (lines:string array) (searchTerm:string) =
    let splitIndex = lines |> Array.findIndex (fun x -> x = searchTerm) |> (+) 1
    let splitLines = (Array.splitAt splitIndex lines).Item2
    let startIndex = 0
    let endIndex = 
        let rec findEndIndex (line:string array) index =
            match line with
            | [| |] -> index
            | _ -> 
                if (Array.head line) = "" then index
                else
                findEndIndex (Array.tail line) (index + 1)
        findEndIndex splitLines 0
    printfn "StartIndex: %A" startIndex
    printfn "EndIndex: %A" endIndex
    printfn "SplitLines: %A" splitLines
    printfn "SplitLinesLength: %A" splitLines.Length
    Array.sub splitLines startIndex endIndex
    |> Array.map (fun x ->
        // printfn "Mapping: %A" x
        let split = x.Split(" ")
        printfn "Split: %A" split
        let sourceRangeStart = split.[1].Trim() |> float
        let destinationRangeStart = split.[0].Trim() |> float
        let rangeLength = split.[2].Trim() |> float
        { SourceRangeStart = sourceRangeStart; DestinationRangeStart = destinationRangeStart; RangeLength = rangeLength }
    )


let soilLines = getMapping lines "seed-to-soil map:"
let fertilizerLine = getMapping lines "soil-to-fertilizer map:"
let waterLine = getMapping lines "fertilizer-to-water map:"
let lightLine = getMapping lines "water-to-light map:"
let temperatureLine = getMapping lines "light-to-temperature map:"
let humidityLine = getMapping lines "temperature-to-humidity map:"
let locationLine = getMapping lines "humidity-to-location map:"

let calculateMapping (mappingInput:MappingInput array) (inputSeed:InputSeed) =
    // printfn "MappingInput: %A" mappingInput
    // printfn "InputSeed: %A" inputSeed
    
    let matchingItem = 
        Array.where (fun x -> 
            let sourceMin = x.SourceRangeStart
            let sourceMax = x.SourceRangeStart + x.RangeLength - float 1
            // printfn "SourceMin: %A" sourceMin
            // printfn "SourceMax: %A" sourceMax
            match inputSeed with
            | InputSeed x when x >= sourceMin && x <= sourceMax -> true
            | _ -> false
            ) mappingInput
    
    let mappedDifference =
        match matchingItem with
        | [| |] -> float 0
        | _ -> matchingItem.[0].SourceRangeStart - matchingItem.[0].DestinationRangeStart

    match inputSeed with
    | InputSeed x -> InputSeed (x - mappedDifference)

let result = 
    Array.map (fun x ->
        calculateMapping soilLines x
        |> calculateMapping fertilizerLine
        |> calculateMapping waterLine
        |> calculateMapping lightLine
        |> calculateMapping temperatureLine
        |> calculateMapping humidityLine
        |> calculateMapping locationLine
        ) seeds
    |> Array.minBy (fun x -> 
        match x with
        | InputSeed x -> x
        )
printfn "Result: %A" result // Result was 173706076