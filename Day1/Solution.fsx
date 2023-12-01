// read lines from file
open System.IO
open System
let lines = File.ReadAllLines("input.txt")

let findFirstDigit (line: string) =
    line |> Seq.find Char.IsDigit |> string

let findLastDigit (line: string) =
    line |> Seq.rev |> Seq.find Char.IsDigit |> string

let findSum lines = 
    Array.fold (fun acc line -> 
        let first = findFirstDigit line
        let last = findLastDigit line
        acc + (int(first + last))
    ) 0 lines

findSum lines