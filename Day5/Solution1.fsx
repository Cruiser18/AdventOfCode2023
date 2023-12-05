open System.IO

let lines = File.ReadAllLines("Day5/testInput.txt")

type MappingInput = {
    SourceRangeStart: int
    DestinationRangeStart: int
    RangeLength: int
}

type InputSeed = InputSeed of int

// mappingDifference = MappingInput.sourceRangeStart - MappingInput.destinationRangeStart
// numbers range mapping rule apply to =  MappingInput.sourceRangeStart + MappingInput.RangeLength 
// outputSeed = inputSeed - mappingDifference

let rawSeedLine = Array.head lines
let seeds = ((rawSeedLine.Split(":").[1]).Trim()).Split(" ") |> Array.map (fun x -> InputSeed (int x))
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
    Array.sub splitLines startIndex endIndex

    |> Array.map (fun x ->
        let split = x.Split(" ")
        let sourceRangeStart = split.[0].Trim() |> int
        let destinationRangeStart = split.[1].Trim() |> int
        let rangeLength = split.[2].Trim() |> int
        { SourceRangeStart = sourceRangeStart; DestinationRangeStart = destinationRangeStart; RangeLength = rangeLength }
    )

let soilLines = getMapping lines "seed-to-soil map:"
let fertilizerLine = getMapping lines "soil-to-fertilizer map:"
let waterLine = getMapping lines "fertilizer-to-water map:"
let lightLine = getMapping lines "water-to-light map:"
let temperatureLine = getMapping lines "light-to-temperature map:"
let humidityLine = getMapping lines "temperature-to-humidity map:"
let locationLine = getMapping lines "humidity-to-location map:"

// let allLines = Array.concat [|soilLines; fertilizerLine; waterLine; lightLine; temperatureLine; humidityLine; locationLine|]

let calculateMapping (mappingInput:MappingInput) (inputSeed:InputSeed) =
    printfn "MappingInput: %A" mappingInput
    printfn "InputSeed: %A" inputSeed
    let mappedDifference = mappingInput.SourceRangeStart - mappingInput.DestinationRangeStart
    let sourceMin = mappingInput.SourceRangeStart
    let sourceMax = mappingInput.SourceRangeStart + mappingInput.RangeLength - 1
    match inputSeed with
    | InputSeed x when x >= sourceMin && x <= sourceMax -> 
        printfn "sourceMin: %A" sourceMin
        printfn "sourceMax: %A" sourceMax
        printfn "Mapped: %A" (x - mappedDifference)
        Some (x - mappedDifference)
    | _ -> 
        printfn "Output is Same: A"
        None

// foreach line in soilLines, match the inputSeed against it. If the Inputseed falls outside the range of the rule, return the inputSeed
Array.map (fun soilLine -> 
    calculateMapping soilLine (InputSeed 1)
) soilLines
|> Array.filter (fun (x:option<int>) -> x.IsSome)


// let mappedSeeds = 
//     Array.map (fun seed -> 
//         Array.map (fun line -> 
//                 calculateMapping line seed
//         ) allLines
//     ) seeds