open System.IO

let lines = File.ReadAllLines("Day6/input.txt")

let getAllDistancesThatBeatRecord (lines:string array) =
    let allTimes = (lines[0].Split(":")[1]).Replace(" ", "") |> float
    let allDistances = (lines[1].Split(":")[1]).Replace(" ", "") |> float

    let doAllPermutations (raceTimeAndDistance: float*float) =
        let totalRaceTime = raceTimeAndDistance |> fst
        let raceDistanceToBeat = raceTimeAndDistance |> snd
        let allTimesPermutations = [|1..(int totalRaceTime)|]
        
        let getDistance (chargeTime:int) (totalRaceTime:float) =
            let remainingTime = totalRaceTime - (float chargeTime)
            let distanceTraveled = (float chargeTime) * remainingTime
            distanceTraveled
        
        let validChargeTimes = 
            Array.where (fun (chargeTime:int) -> 
                (getDistance chargeTime totalRaceTime) > raceDistanceToBeat
                ) allTimesPermutations
        validChargeTimes.Length
    doAllPermutations (allTimes, allDistances)
let result = getAllDistancesThatBeatRecord lines // Answer was 33875953