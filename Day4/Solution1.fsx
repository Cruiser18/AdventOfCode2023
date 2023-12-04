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

let pointsForCard (card:Card) = 
    let points = 
        Array.fold (fun acc x -> 
        if Array.contains x card.WinningNumbers then acc * 2 else acc
        ) 1 card.Numbers
    (points / 2)

let cards = Array.map lineToCard lines
let pointsForCards = Array.map pointsForCard cards
Array.sum pointsForCards // Result is 21485