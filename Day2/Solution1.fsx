open System.IO

type GameNumber = GameNumber of int
type Color = Blue of int | Red of int | Green of int

let redLimit = 12
let blueLimit = 14
let greenLimit = 13

type Game = {
    GameNumber: GameNumber
    Colors: Color array
}

let lines = File.ReadAllLines("Day2/input.txt")

let getColorAndValue (x:string array) : Color =
    match (Array.toList x) with
    | [] -> failwith "no color"
    | head::tail -> 
        match head with
        | "blue" -> Blue (tail.[0] |> int)
        | "red" -> Red (tail.[0] |> int)
        | "green" -> Green (tail.[0] |> int)
        | _ -> failwith "no color"

let colorArray (line:string) =
    let value = 
        (line.Split([|':'|])[1]).Split([|';'|])
        |> Array.map (fun x ->
            let numberAndColor = 
                x.Split([|','|])
                |> Array.map (fun y -> y.Trim())
                |> Array.map (fun z -> z.Split([|' '|]))
            let colorsAndValues = 
                Array.map (fun (y:string array) -> 
                    getColorAndValue (Array.rev y)
                ) numberAndColor
            colorsAndValues
        )
        |> Array.concat
    value

let getvalidGames (x:Game array) = 
    Array.filter (fun (x:Game) ->
        let colors = x.Colors
        let isColorWithinLimit color =
            match color with
            | Blue value -> value <= blueLimit
            | Red value -> value <= redLimit
            | Green value -> value <= greenLimit
        Array.forall isColorWithinLimit colors
    ) x

let result =
    Array.map (fun (x:string) ->
        let gameNumberWithNumber = (x.Split([|':'|])[0]).Split([|' '|])[1] |> int |> GameNumber
        let game = {
            GameNumber = gameNumberWithNumber
            Colors = (colorArray x)
        }
        game
    ) lines
    |> getvalidGames
    |> Array.sumBy (fun x -> 
            match x.GameNumber with
            | GameNumber value -> value
        )
result // answer was 2776