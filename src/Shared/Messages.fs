namespace Shared

module MessageTypes =

    open DataTypes

    type Msg =
        | Instruction of Instruction
        | GameData of GameData
        | WriteToConsole of ConsoleMessage
        | PlayerMessage of PlayerMessage //Recursive data-type, wrapping up a Msg
        | SysMsg of SysMsg

    and Instruction =
        | NewPlayer //of Player
        | UpdatePlayerName of string
        | AdoptPlayer of PlayerId
        | UpdatePlayer of Player
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
        | SendAllPlayers

    and GameData =
        | GameNums of GameNumbers
        | ScoreUpdate of Score
        | HighScore of Score
        | ScoreLogs of ScoreLog list
        | Fail of FailMessage
        | NewRandom of int
        | Players of PlayerList


    and FailMessage =
        | TooManyNumbers
        | OverTen
        | Killed
        | Ended

    and PlayerMessage =
            {msg : Msg
             sender: Player}

    and SysMsg =
        | SetChannelSocketId of SocketId
        | ConnectionChange of ConnectionState
        | SetPlayerId of PlayerId
        | ChangeView of ViewState
        | KeyPress of string
        | CloseEvent of SocketId
        | PlayerUpdate of Player

    and  ConnectionState =
        | DisconnectedFromServer
        | ConnectedToServer of WsSender
        | Connecting

        member this.IsConnected =
            match this with
            | ConnectedToServer _ -> true
            | DisconnectedFromServer | Connecting -> false

    and WsSender = Msg -> Unit

