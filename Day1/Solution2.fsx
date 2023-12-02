// read lines from file
open System.IO
let lines = File.ReadAllLines("input.txt")
let findMatchingIndexes (str: string) =
    let earlyLetterNumbersFound = 
        [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
        |> Array.mapi (fun i number -> 
            let index = str.IndexOf number
            (index, (string i))
        )
        |> Array.filter (fun (index, number) -> index > -1)
    let LatestLetterNumbersFound = 
        [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
        |> Array.mapi (fun i number -> 
            let index = str.LastIndexOf number
            (index, (string i))
        )
        |> Array.filter (fun (index, number) -> index > -1)
    let earlyNumbersFound = 
        [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]
        |> Array.map (fun number -> 
            let earliesIndex = str.IndexOf number
            (earliesIndex, number)
        )
        |> Array.filter (fun (index, number) -> index > -1)
    let LastNumbersFound = 
        [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]
        |> Array.map (fun number -> 
            let earliesIndex = str.LastIndexOf number
            (earliesIndex, number)
        )
        |> Array.filter (fun (index, number) -> index > -1)
    Array.concat [earlyLetterNumbersFound;LatestLetterNumbersFound;earlyNumbersFound;LastNumbersFound]
    
findMatchingIndexes "9964pfxmmr474"

let findFirstNumber (line:(int*string) array) =
    Array.minBy (fun (index, number) -> index) line |> snd

let findLastNumber (line:(int*string) array) =
    Array.maxBy (fun (index, number) -> index) line |> snd

let findSum lines = 
    Array.fold (fun acc line -> 
        let matchingIndexes = findMatchingIndexes line
        let first = findFirstNumber matchingIndexes
        let last = findLastNumber matchingIndexes
        printfn "%s %s" first last
        acc + (int(first + last))
    ) 0 lines

findSum lines // Answer was 55429