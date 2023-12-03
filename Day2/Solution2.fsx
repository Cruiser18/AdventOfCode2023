open System.IO

type GameNumber = GameNumber of int
type Color = Blue of int | Red of int | Green of int

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

let findColorWithHighestValue (colors: Color array) =
    colors
    |> Array.groupBy (fun color ->
        match color with
        | Blue _ -> "Blue"
        | Red _ -> "Red"
        | Green _ -> "Green"
    )
    |> Array.map (fun (key, group) ->
        let maxColor =
            group
            |> Array.maxBy (fun color ->
                match color with
                | Blue value | Red value | Green value -> value
            )
        key, maxColor
    )

let result =
    Array.map (fun (x:string) ->
        let gameWithNumber = (x.Split([|':'|])[0]).Split([|' '|])[1] |> int |> GameNumber
        let game = {
            GameNumber = gameWithNumber
            Colors = (colorArray x)
        }
        game
    ) lines
    |> Array.map (fun (x: Game) ->
        findColorWithHighestValue x.Colors
    )
    |> Array.map (fun x -> 
        Array.fold (fun acc (key, value) ->
            match value with
            | Blue value | Red value | Green value -> acc * value
        ) 1 x
    )
    |> Array.sum
result // Answer was 68638