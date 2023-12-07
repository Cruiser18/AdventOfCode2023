open System.IO

type Hand = { Hand: string; Bid: int }

// Input is something like:
// Hand & Bid
// 32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483

// The output is the strength of the ranking of the hand * bid.
// There's a 1000 lines in the full input, so the potential best hand will be 1000 * the bid

// I must sort all the hands along with their Bid in order of strength
// Then I can multiply each hand by its bid and sum them all up

let isFiveOfAKind (input: string) =
    let firstChar = input.[0]
    input.[1..] |> Seq.forall (fun c -> c = firstChar)

let isFourOfAKind (input: string) =
    let charCounts =
        input
        |> Seq.groupBy id
        |> Seq.map (fun (c, chars) -> c, Seq.length chars)
        |> Map.ofSeq

    charCounts
    |> Map.exists (fun _ count -> count = 4)

let isFullHouse (input: string) =
    let charCounts =
        input
        |> Seq.groupBy id
        |> Seq.map (fun (c, chars) -> c, Seq.length chars)
        |> Map.ofSeq

    charCounts
    |> Map.exists (fun _ count -> count = 3)
    && charCounts
        |> Map.exists (fun _ count -> count = 2)

let isThreeOfAKind (input: string) =
    let charCounts =
        input
        |> Seq.groupBy id
        |> Seq.map (fun (c, chars) -> c, Seq.length chars)
        |> Map.ofSeq

    charCounts
    |> Map.exists (fun _ count -> count = 3)


let isTwoPairs (input: string) =
    let charCounts =
        input
        |> Seq.groupBy id
        |> Seq.map (fun (c, chars) -> c, Seq.length chars)
        |> Map.ofSeq

    charCounts
    |> Map.filter (fun _ count -> count = 2)
    |> Map.count = 2

let isOnePair (input: string) =
    let charCounts =
        input
        |> Seq.groupBy id
        |> Seq.map (fun (c, chars) -> c, Seq.length chars)
        |> Map.ofSeq

    charCounts
    |> Map.exists (fun _ count -> count = 2)


let isHighCard (input: string) =
    let distinctChars = input |> Seq.distinct
    Seq.length distinctChars = Seq.length input


type HandTypes = 
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPairs
    | OnePair
    | HighCard
    | NoMatch

let matchHand (input: string) =
    match input with
    | _ when isFiveOfAKind input -> FiveOfAKind
    | _ when isFourOfAKind input -> FourOfAKind
    | _ when isFullHouse input -> FullHouse
    | _ when isThreeOfAKind input -> ThreeOfAKind
    | _ when isTwoPairs input -> TwoPairs
    | _ when isOnePair input -> OnePair
    | _ when isHighCard input -> HighCard
    | _ -> NoMatch

let getHandStrength (hand:string) =
    let handStrength = 
        let handmatch = matchHand hand
        match handmatch with
        | FiveOfAKind -> 7
        | FourOfAKind -> 6
        | FullHouse -> 5
        | ThreeOfAKind -> 4
        | TwoPairs -> 3
        | OnePair -> 2
        | HighCard -> 1
        | _ -> 0
    handStrength

let lines = File.ReadAllLines("Day7/testInput.txt")

let hands = 
    Array.map (fun (x:string) -> 
        let firstAndSecond = x.Split(" ")
        { Hand = firstAndSecond[0]; Bid = int firstAndSecond[1] }
        ) lines

let result:Hand array = 
    hands 
    |> Array.sortInPlaceWith (fun (x:Hand) (y:Hand) -> 
        let xStrength = x.Hand |> getHandStrength
        let yStrength = y.Hand |> getHandStrength
        let c = compare xStrength yStrength
        if c <> 0
        then 
            c 
        elif x.Hand = y.Hand then 0        
        else
            let getCardStrength (x:char) =
                match x with
                | 'A' -> 14
                | 'K' -> 13
                | 'Q' -> 12
                | 'J' -> 11
                | 'T' -> 10
                | _ -> int x - 48
            let rec findGreater (input:(char*char) array) =
                let x, y = input |> Array.head
                let xStrength = getCardStrength x
                let yStrength = getCardStrength y
                printfn "x: %A, y: %A" xStrength yStrength
                let c = compare xStrength yStrength
                if c <> 0 then c else findGreater (input |> Array.tail)
            let zipped = 
                seq y.Hand
                |> Seq.zip x.Hand
                |> Seq.toArray
            findGreater zipped
    )
    hands
let rec calculate (acc:int) (x:Hand list) (index:int) =
    match x with
    | [] -> acc
    | head::tail ->
        let newAcc = head.Bid * index
        calculate (acc + newAcc) tail (index + 1)
let finalResult = calculate 0 (Array.toList result) 1
finalResult // 250602641 is the right answer