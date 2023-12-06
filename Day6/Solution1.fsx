open System.IO

let lines = File.ReadAllLines("Day6/input.txt")

let getAllDistancesThatBeatRecord (lines:string array) =
    let allTimes = ((lines[0].Split(":")[1]).Trim()).Split(" ") |> Array.where (fun x -> x <> "") |> Array.map (fun x -> x.Trim() |> int)
    let allDistances = ((lines[1].Split(":")[1]).Trim()).Split(" ") |> Array.where (fun x -> x <> "") |>  Array.map (fun x -> x.Trim() |> int)
    let timesAndDistances = Array.zip allTimes allDistances

    let doAllPermutations raceTimeAndDistance =
        let totalRaceTime = raceTimeAndDistance |> fst
        let raceDistanceToBeat = raceTimeAndDistance |> snd
        let allTimesPermutations = [|1..totalRaceTime|]
        
        let getDistance chargeTime totalRaceTime =
            let remainingTime = totalRaceTime - chargeTime
            let distanceTraveled = chargeTime * remainingTime
            distanceTraveled
        
        let validChargeTimes = 
            Array.where (fun chargeTime -> 
                (getDistance chargeTime totalRaceTime) > raceDistanceToBeat
                ) allTimesPermutations
        validChargeTimes.Length

    let allPermutations = 
        Array.map doAllPermutations timesAndDistances
        |> Array.fold (fun acc x -> acc * x) 1
    allPermutations
let result = getAllDistancesThatBeatRecord lines // Answer was 281600