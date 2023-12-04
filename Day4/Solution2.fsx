open System.IO
open System
let lines = File.ReadAllLines("day4/input.txt")

type Card = {
    CardNumber: int
    WinningNumbers: int array
    Numbers: int array
}

let lineToCard (x:string) =
    printfn "%s" x
    let cardNumber = ((x.Split(":")[0]).Replace("Card", "")).Trim() |> int
    let winningNumbers = ((x.Split("|")[0]).Split(":")[1]).Trim().Split(" ") |> Array.map (fun x -> x.Trim()) |> Array.where (fun x -> x <> "") |> Array.map int
    let numbers = (x.Split("|")[1]).Trim().Split(" ") |> Array.where (fun x -> x <> "") |> Array.map int
    { CardNumber = cardNumber; WinningNumbers = winningNumbers; Numbers = numbers }

let copiesFromCard (card:Card) = 
    let points = 
        Array.fold (fun acc x -> 
        if Array.contains x card.WinningNumbers then acc + 1 else acc
        ) 0 card.Numbers
    [|for i in card.CardNumber + 1 .. card.CardNumber + points -> i|]

let cardRows = Array.map lineToCard lines

let result cardRows =
    let rec iterateOverRows (lines: Card array) (copiesArray:int array) (acc:int) =
        match lines with
        | [||] -> acc
        | _ ->
            let firstLineArray = 
                let first = Array.head lines
                [|first|]

            let count = 
                let matchingNumbers = Array.where (fun x -> x = (Array.head lines).CardNumber) copiesArray
                Array.length matchingNumbers + 1
            // printfn "%A" count
            let newCopiesArray = 
                [|for i in 1 .. count -> Array.collect copiesFromCard firstLineArray|]
                |> Array.concat
                |> Array.append copiesArray
            // printfn "NewCopies: %A" newCopiesArray
            iterateOverRows (Array.tail lines) newCopiesArray (acc + count)
    iterateOverRows cardRows [||] 0
result cardRows // Answer is 11024379