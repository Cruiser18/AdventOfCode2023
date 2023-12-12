open System.IO

let lines = File.ReadAllLines("Day11/testInput.txt")

let insertRowIn2DArrayAt (index:int) (inputArray:int array) (test:int[,]) =
    let newArray = Array2D.init (Array2D.length1 test + 1) (Array2D.length2 test) (fun i j ->
        if i = index then inputArray[j] // New row
        elif i > index then test.[i - 1, j] // Existing rows
        else test.[i, j] // Existing rows
    )
    newArray

let insertColumnIn2DArrayAt (index:int) (inputArray:int array) (test:int[,]) =
    let newArray = Array2D.init (Array2D.length1 test) (Array2D.length2 test + 1) (fun i j ->
        if j = index then inputArray[i] // New column
        elif j > index then test.[i, j - 1] // Existing columns
        else test.[i, j] // Existing columns
    )
    newArray

let create2DArray (lines:string array) =
    let length = lines.Length
    let width = lines.[0].Length
    let newArray = Array2D.init length width (fun i j ->
        let char = lines.[i].[j]
        if char = '.' then 0
        elif char = '#' then 1
        else -1
    )
    newArray
let inputAs2DArray = create2DArray lines

// foreach row in inputAs2DArray check if all values are 0 and return their index
let ifRowContainsOnlyZeroReturn0Else1 x index = if (Array.sum x = 0) then 0 else index

let getRowIndexWhereOnlyZero = 
    let indexes = 
        let newArray = Array.create (Array2D.length1 inputAs2DArray) 0
        for i in 0..(Array2D.length2 inputAs2DArray)-1 do
            newArray[i] <- inputAs2DArray.[0, i]
        if newArray |> Array.sum = 0 then 1 else 0
    

getRowIndexWhereOnlyZero

let testArray = 
    Array2D.mapi (fun i j x -> 
    if i = 0 && j = 4 then 1
    else x
    ) inputAs2DArray