open System.IO

let lines = File.ReadAllLines("Day8/input.txt")
let rawNodesList = lines[2..]
let rawMoves = lines[0] |> Seq.toList

// let nodesThatEndsWithZ = 
//     rawNodesList 
//     |> Array.map (fun y ->
//         let leftAndRight = y.Split("(")[1]
//         let left = leftAndRight.Split(",")[0]
//         let right = ((leftAndRight.Split(",")[1]).Trim().Substring(0, 3))
//         if left.EndsWith("Z") then left 
//         elif right.EndsWith("Z") then right
//         else ""
//     )
//     |> Array.filter (fun y -> y <> "")

// let result = nodesThatEndsWithZ

let getNextNode (nodeName:string) =
    let findNodeAsString = rawNodesList |> Array.find (fun y -> 
        let name  = (y.Split("=")[0]).Trim()
        name = nodeName
        )
    let leftAndRight = findNodeAsString.Split("(")[1]
    let left = leftAndRight.Split(",")[0]
    let right = ((leftAndRight.Split(",")[1]).Trim().Substring(0, 3))
    (nodeName, left, right)

let nodesThatEndsWithA = 
    rawNodesList |> Array.filter (fun y -> 
    let name  = (y.Split("=")[0]).Trim()
    name.EndsWith("A")
    )
    |> Array.map (fun y -> 
        let name  = (y.Split("=")[0]).Trim()
        name
        )
    |> Array.toList

let findNumSteps (initialMoves:char list) (nodes:string list) =
    let rec stepThroughNodes (nextMoves:char list) numSteps (currentNodes:string list) =
        // printfn "%A" nextMoves
        // printfn "%A" numSteps
        match nextMoves with
        | head::tail -> 
            match head with
            | 'L' ->
                let newNodesList = 
                    currentNodes |> List.map (fun x -> 
                        let (head, left, right) = getNextNode x
                        left
                    )
                printfn "%A" newNodesList
                let isTrue = newNodesList |> List.forall (fun (x:string) -> x.EndsWith("Z"))
                if isTrue then numSteps else stepThroughNodes tail (numSteps + 1) newNodesList

            | 'R' -> 
                let newNodesList = 
                    currentNodes |> List.map (fun x -> 
                        let (head, left, right) = getNextNode x
                        right
                    )
                printfn "%A" newNodesList
                let isTrue = newNodesList |> List.forall (fun (x:string) -> x.EndsWith("Z"))
                if isTrue then numSteps else stepThroughNodes tail (numSteps + 1) newNodesList
            | _ -> 
                failwith "An error occurred."
        // | [] when numSteps > 10 -> numSteps
        | [] -> 
            printfn "Starting over at steps %A" numSteps
            stepThroughNodes initialMoves numSteps currentNodes
    stepThroughNodes initialMoves 1 nodes
findNumSteps rawMoves nodesThatEndsWithA