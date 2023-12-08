open System.IO

let lines = File.ReadAllLines("Day8/testInput.txt")

let moves = lines[0] |> Seq.toArray

type Node = {
    Name: string
    Left: string
    Right: string
}

let rawNodesList = lines[2..]

let nodesNameList = rawNodesList |> Array.map (fun x -> ((x.Split("="))[0]).Trim())


    
// Create list of nodes without children
// For every node, create node and input its left and right child
// To do this, I will have to look up all nodes for from list and insert its reference