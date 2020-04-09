namespace Shared
open System
open Browser.Types

module TensTypes =

    type Score = Score of int

    let getScoreValue(Score i) = i


    type PlayerId = PlayerId of int option

    let setPlayerId i = PlayerId (Some i)
    let getPlayerId (PlayerId i) = match i with
                                        | Some x -> x
                                        | None -> -1


    type PlayerName = PlayerName of string option

    let setPlayerName s = PlayerName (Some s)
    let getPlayerName (PlayerName n) = match n with
                                        | Some s -> s
                                        | None -> ""

    type SocketID = SocketID of Guid

    let setSocketID s = SocketID s
    let getSocketID (SocketID n) = n


    type Player =
        {
        playerName:Option<string>
        playerId:Option<int>
        socketId:Option<SocketID>
        }

    type Extras =
        {
         HighScore : int
        }

    type GameConfig =
        {
         targetValue:int
         maxAllowableClicked:int
        }

    type ViewState =
    | SimpleView
    | AdvancedView

    type ClickedNumberIndex =
        { number: int
          listIndex : int}

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

    type LengthAndSum = {length : int
                         sum : int}

    let getLengthAndSum(input:GameNumbers) =
        match input with
        | RandomNumbers(data) | ClickedNumbers(data)-> data.Length, data |> List.sum
        
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
        | UpdatePlayerName of string
        | DeleteAllOtherPlayers of Player
        | StartGame
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
        | ChangeView of ViewState


    type FailMessage =
        | TooManyNumbers //From RandomHandler
        | OverTen //From ClickedHandler
        | HardStop //of Player //From User interface




    type GameData =
        | GameNums of GameNumbers
        | ScoreUpdate of Score
        | HighScore of Score
        | SetChannelSocketId of SocketID
        | SetWebSocket of WebSocket
        | SetPlayerId of int
        | Fail of FailMessage // Internal (Server)
        | NewRandom of int //Internal (Server) - from random generator to RandomHandler



    type Msg =
        | Instruction of Instruction
        | GameData of GameData
        | WriteToConsole of ConsoleMessage
        | PlayerMessage of PlayerMessage

    and PlayerMessage =
            {msg : Msg
             plyr: Player}



              








