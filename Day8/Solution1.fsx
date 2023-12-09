open System.IO

let lines = File.ReadAllLines("Day8/input.txt")
let rawNodesList = lines[2..]
let moves = lines[0] |> Seq.toList

let getNextNode (nodeName:string) =
    let findNodeAsString = rawNodesList |> Array.find (fun y -> 
        let name  = (y.Split("=")[0]).Trim()
        name = nodeName
        )
    let leftAndRight = findNodeAsString.Split("(")[1]
    let left = leftAndRight.Split(",")[0]
    let right = ((leftAndRight.Split(",")[1]).Trim().Substring(0, 3))
    (nodeName, left, right)

let findNumSteps (moves:char list) (node:string) =
    let rec test (nextMoves:char list) numSteps (currentNode:string) =
        match nextMoves with
        | head::tail -> 
            match head with
            | 'L' -> 
                let (head, left, right) = getNextNode currentNode
                if left = "ZZZ" then numSteps else test tail (numSteps + 1) left
            | 'R' -> 
                let (head, left, right) = getNextNode currentNode
                if right = "ZZZ" then numSteps else test tail (numSteps + 1) right
            | _ -> 
                test tail numSteps currentNode
        | [] when numSteps = 10 -> numSteps
        | [] -> 
            test moves numSteps currentNode
    test moves 1 node

printfn "%A" moves
printfn "%A" rawNodesList
findNumSteps moves "AAA"


    
// Create list of nodes without children
// For every node, create node and input its left and right child
// To do this, I will have to look up all nodes for from list and insert its reference