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

let testCard1 = lineToCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
let testCard2 = lineToCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

let array1 = copiesFromCard testCard1
let array2 = copiesFromCard testCard2

let findDuplicates (arr: int array) =
    let rec findDuplicatesHelper (input: int list) (seen: Set<int>) (result: int list) =
        match input with
        | [] -> result
        | x :: xs ->
            if Set.contains x seen then
                findDuplicatesHelper xs seen (x :: result)
            else
                findDuplicatesHelper xs (Set.add x seen) result

    findDuplicatesHelper (List.ofArray arr) Set.empty []

let testArray = Array.concat [|array1;array2|] |> Array.groupBy (fun x -> x) 



let cardRow = Array.map lineToCard lines



let pointsForCards = Array.map copiesFromCard cardRow
Array.sum pointsForCards 