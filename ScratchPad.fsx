let test = Array2D.create 4 10 2

let index = 2
let inputArray = [|1;2;3;4;5;6;7;8;9;10|]

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

let myNewArray = insertRowIn2DArrayAt 2 inputArray test

let verticalInputArray = [|1;2;3;4|]
let myNewArray2 = insertColumnIn2DArrayAt 2 verticalInputArray test