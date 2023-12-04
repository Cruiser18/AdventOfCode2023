open System.IO
open System

let specialChars = [|"*";"#";"+";"$";"=";"@";"%";"/";"&";"-"|]
let lines = File.ReadAllLines("Day3/testInput.txt")

type NumberContainer = {
    Number: int
    LineNumber: int
    IndexStart: int
    IndexEnd: int
}

type GetNumbersFromLine = string -> int -> NumberContainer array
let getNumbersFromLine:GetNumbersFromLine = 
    fun line lineNumber ->
        
        [||]

let rec test x arrayOfNumbers foundAtIndex =
    match x with
    | [] -> ()
    | head::tail when head = "." -> test tail arrayOfNumbers 0
    | head::tail when head = "." && foundAtIndex <> 0 -> 
        test tail arrayOfNumbers 0
    | head::tail when Char.IsDigit(char head) && foundAtIndex = 0 -> 
        printfn "%s" head
        test tail arrayOfNumbers (foundAtIndex + 1)
    
let rec test2 (x:string) arrayOfNumbers number =
    match x with
    // | Seq.empty -> arrayOfNumbers
    | head::tail when head = "." -> test2 tail arrayOfNumbers ""
    | head::tail when Char.IsDigit(char head) -> 
        test2 tail arrayOfNumbers (number + head)

test2 "467..114.." [||] ""