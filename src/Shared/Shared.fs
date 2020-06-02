namespace Shared
open System


module TensTypes =

    type Score = Score of int

    let getScoreValue(Score i) = i

    type ScoreLog = {playerName:string; actorName:string; highScore: Score}


    type PlayerId = PlayerId of int option

    let setPlayerId i = PlayerId (Some i)
    let getPlayerId (PlayerId i) = i


    type PlayerName = PlayerName of string option

    let setPlayerName s = PlayerName (Some s)
    let getPlayerName (PlayerName n) = match n with
                                        | Some s -> s
                                        | None -> ""



    type SocketID = SocketID of Guid

    let setSocketID s = SocketID s
    let getSocketID (SocketID n) = n

    //let getOptionSocketID (s:SocketID option) = match s with
    //                                            | Some z -> getSocketID z
    //                                            | None -> Guid.Empty


    type Player =
        {
        playerName:PlayerName
        playerId:PlayerId
        socketId:SocketID
        orphaned:Boolean
        }

    let getSafePlayerName (p:Player) = (getPlayerName p.playerName).Replace(" ", "_") + "_" + string (match getPlayerId p.playerId with | Some n -> n | None -> -1)

    type GameSystemData =
        {
         PlayerHighScore : Score
         SystemHighScores : ScoreLog list
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

    let getNumbers(RandomNumbers(data)|ClickedNumbers(data)) = data

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

    type Msg =
        | Instruction of Instruction
        | GameData of GameData
        | WriteToConsole of ConsoleMessage
        | PlayerMessage of PlayerMessage //Recursive data-type, wrapping up a Msg
        | SysMsg of SysMsg

    and Instruction =
        | NewPlayer of Player
        | UpdatePlayerName of string
        | DeleteAllOtherPlayers of Player
        | KillMeNow
        | ReRegister
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

        //| ReceivedFromServer of Msg
        //| MessageChanged of string

    and GameData =
        | GameNums of GameNumbers
        | ScoreUpdate of Score
        | HighScore of Score
        | ScoreLogs of ScoreLog list
        | Fail of FailMessage
        | NewRandom of int

    and FailMessage =
        | TooManyNumbers //From RandomHandler
        | OverTen //From ClickedHandler
        | HardStop
        | Ended

    and PlayerMessage =
            {msg : Msg
             sender: Player}

    and SysMsg =
        | SetChannelSocketId of SocketID
        | ConnectionChange of ConnectionState
        | SetPlayerId of int
        | ChangeView of ViewState
        | KeyPress of string
        | CloseEvent of SocketID

    and  ConnectionState =
        | DisconnectedFromServer
        | ConnectedToServer of WsSender
        | Connecting

        member this.IsConnected =
            match this with
            | ConnectedToServer _ -> true
            | DisconnectedFromServer | Connecting -> false

    and WsSender = Msg -> Unit

