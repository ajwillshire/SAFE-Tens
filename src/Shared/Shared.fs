namespace Shared
open System
open Browser.Types

module TensTypes =

    type Player =
        {
        socketId:Option<Guid>
        playerId:Option<int>
        playerName:Option<string>
        playerWs:Option<WebSocket>
        }


    type ClickedNumberIndex =
        { number: int
          listIndex : int}

    //type Score = Score of int

    type msgAndColour =  {msg:string; colour:int}

    type ConsoleMessage =
            | Simple of string
            | Complex of msgAndColour
            | Error of exn

    

module CommTypes =

    open TensTypes

    type GameNumbers =
        | RandomNumbers of int list
        | ClickedNumbers of int list

    let getNumberByIndex(RandomNumbers(data)|ClickedNumbers(data)) i = data.[i]

    let ForEachNumber(RandomNumbers(data)|ClickedNumbers(data)) dothis = data |> List.mapi dothis

    let addToGameNumbers(input:GameNumbers) newNum =
        match input with
        | RandomNumbers(data) -> RandomNumbers(data @ [newNum])
        | ClickedNumbers(data) -> ClickedNumbers(data @ [newNum])

    let getLengthAndSum(input:GameNumbers) =
        match input with
        | RandomNumbers(data) | ClickedNumbers(data)-> (data.Length), (data |> List.sum)
        
    let removeAtIndex index list =
        if index < (list |> List.length) then
          list 
          |> List.mapi (fun i element -> (i <> index, element))
          |> List.filter fst 
          |> List.map snd
        else
          list


    let removeFromGameNumbers(input:GameNumbers) (newNum:ClickedNumberIndex) =
        match input with
        | RandomNumbers(data) -> RandomNumbers(removeAtIndex newNum.listIndex data)
        | ClickedNumbers(data) -> ClickedNumbers(removeAtIndex newNum.listIndex data)



module MessageTypes =

    open CommTypes
    open TensTypes

    type Instruction =
        | NewPlayer of Player
        | NewGame
        | StartGame
        | RestartGame
        | StopGame //- Issued from the server to the client
        | StartRandom
        | StopRandom
        | NewClickedNumber of ClickedNumberIndex
        | ClearNumbers 
        | SingleAuto 
        | StartAuto 
        | StopAuto
        | RemoveNumber of ClickedNumberIndex //Internal (Server)
        | IncrementScore of int //Internal (Server)
        | SendMeNumbers //Internal (Server)
        | Poke //Internal (Server)


    type FailMessage =
        | TooManyNumbers //From RandomHandler
        | OverTen //From ClickedHandler

    type Score = Score of int

    let scoreValue(Score i) = i


    type GameData =
        | GameNums of GameNumbers
        | ScoreUpdate of Score
        | HighScore of Score
        | SetChannelSocketId of Guid
        | SetWebSocket of WebSocket
        | SetPlayerId of int
        | Fail of FailMessage // Internal (Server)
        | NewRandom of int //Internal (Server) - from random generator to RandomHandler



    type Msg =
        | Instruction of Instruction
        | GameData of GameData
        | WriteToConsole of ConsoleMessage


    type PlayerMessage =
        {msg : Msg
         plyr: Player}












