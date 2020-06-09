module Model

open Elmish
open System

//Solution References
open Shared.DataTypes
open Shared.MessageTypes
open Thoth.Fetch

//These types are purely local to the Elmish part of the Application
type Running =
  {
    Clicked : GameNumbers
    Numbers : GameNumbers
    Points : Score
  }

type GameOver =
    {
     FinalScore : Score
     FailReason : FailMessage
     Culprit : Player option
    }

type ModelState =
    | NotStarted
    | Running of Running 
    | FinishedGame of GameOver

type BroadcastMode = ViaWebSocket | ViaHTTP

type Model =
    {
    Player : Player
    ModelState : ModelState
    GameSystemData : GameSystemData
    ViewState : ViewState
    ConnectionState : ConnectionState
    CommunicationMode: BroadcastMode
    }

//Send messages via API
let sendPMToServerAPI(game:Model) (msg:Msg) = let pm = PlayerMessage {msg = msg; sender = game.Player}
                                              Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", pm)) () //Thing to do
                                                                   (fun(m) -> (Simple >> WriteToConsole) (string m)) //Message if it succeeds (gets response from Server-side)
                                                                   (fun(e) -> (Error >> WriteToConsole) e)          //Message if it fails

//Send messages via with websockets
let sendPMToServerWS(game:Model) (msg:Msg) = let pm = PlayerMessage {msg = msg; sender = game.Player}
                                             match game.ConnectionState with
                                                            | ConnectedToServer s -> s pm |> ignore
                                                            | _ -> ()
                                             Cmd.ofMsg ((Simple >> WriteToConsole) "Message sent on WS")

// ************************************************************

let sendMessageToServer(game:Model) (msg:Msg) =
    match game.CommunicationMode with
    | BroadcastMode.ViaWebSocket -> (sendPMToServerWS game msg)
    | BroadcastMode.ViaHTTP -> (sendPMToServerAPI game msg)

//Shorthand for returning both the game and the command
let forwardMessageToServer (game:Model) (msg:Msg) = game, sendMessageToServer game msg

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.

let private noPlayer = {SocketId = SocketId None; PlayerId = PlayerId None; PlayerName = PlayerName None; ActorName = ActorName None; Orphaned = false; HighScore = Score 0}
let private blankSystemData = {PlayerHighScore = Score 0; SystemHighScores = []; Players = None}
let private newGame (model:Model) = {model with ModelState = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = Score 0}}
let private initialModel = {ModelState = NotStarted; Player = noPlayer; GameSystemData = blankSystemData; ViewState = SimpleView; ConnectionState = DisconnectedFromServer; CommunicationMode = BroadcastMode.ViaWebSocket}

let private withoutCommands model = model, Cmd.none


//Process Instruction messages
let handleInstructions(msg:Instruction) (game:Model) : Model * Cmd<Msg> =
    match game.ModelState, msg with
                    | NotStarted, UpdatePlayerName s ->
                        withoutCommands <| {game with Player = {game.Player with PlayerName = setPlayerName(s)}}
    
                    | NotStarted, NewPlayer ->
                        Console.WriteLine "Sending NewPlayer command..."
                        forwardMessageToServer game (Msg.Instruction msg)
    
                    | NotStarted, AdoptPlayer _ ->
                        forwardMessageToServer game (Msg.Instruction msg)
    
                    //State can be either NotStarted or FinishedGame
                    | _, StartGame ->
                        forwardMessageToServer (newGame game) (Msg.Instruction msg)
    
                    //This is more unusual because we need to use the existing playerId to rout the message correctly while replacing it with a new blank Id.
                    //Note that the socketId remains constant.
                    | _, KillMeNow ->   let cmd1 = sendMessageToServer game (Msg.Instruction msg)
                                        let cmd2 = Cmd.ofMsg(Instruction ReRegister)
                                        game, Cmd.batch[cmd1;cmd2]

                    //Returns the player to a blank slate except for the existing SocketId
                    | _, ReRegister -> {game with ModelState = NotStarted
                                                               Player = {noPlayer with SocketId = game.Player.SocketId}
                                                               GameSystemData = blankSystemData}, Cmd.none
    
                    //All other commands get sent to the Server for processing
                    | _, _ -> forwardMessageToServer game (Msg.Instruction msg)

//Process GameData messages
let handleData(msg : GameData) (game : Model) : Model * Cmd<Msg> =

    match game.ModelState, msg with
                | Running state, GameNums ints ->
                    match ints with
                    | RandomNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Numbers = ints}}
                    | ClickedNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Clicked = ints}}

                | Running state, ScoreUpdate p ->
                    Console.WriteLine (sprintf "Current Score: %i" (getScoreValue p))
                    withoutCommands <| {game with ModelState = Running {state with Points = p}}

                | _, HighScore h -> withoutCommands <| {game with GameSystemData = {game.GameSystemData with PlayerHighScore = h}}

                | _, ScoreLogs l -> withoutCommands <| {game with GameSystemData = {game.GameSystemData with SystemHighScores = l}}

                | _, Players ps -> withoutCommands <| {game with GameSystemData = {game.GameSystemData with Players = Some ps}}

                | Running state, Fail f ->
                    let finishedGame = {game with ModelState = FinishedGame {FinalScore= state.Points; FailReason = f; Culprit = None}}
                    match f with
                    | TooManyNumbers -> finishedGame, Cmd.ofMsg("Too many numbers in random set." |> (Simple >> WriteToConsole))
                    | OverTen -> finishedGame, Cmd.ofMsg("The sum of total clicked numbers exceeded ten." |> (Simple >> WriteToConsole))
                    | Killed -> finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by someone.") |> (Simple >> WriteToConsole))
                    | Ended -> finishedGame, Cmd.ofMsg((sprintf "You quit!") |> (Simple >> WriteToConsole))

                | _, Fail f -> let finishedGame = {game with ModelState = FinishedGame {FinalScore= Score 0; FailReason = f; Culprit = None}}
                               match f with
                               | Killed -> finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by someone.") |> (Simple >> WriteToConsole))
                               | _ -> withoutCommands <| game


                | _ -> withoutCommands <| game

//Process SysMsg messages 
let handleSystemMessages (msg:SysMsg) (game:Model) =
    Console.WriteLine "System Message received"
    match game.ModelState, msg with
            | _, PlayerUpdate p ->  Console.WriteLine "Player Update received"
                                    withoutCommands <| {game with Player = p}
                                        //match state with
                                        //| NotStarted -> withoutCommands <| {game with Player = p}
                                        //| _ -> withoutCommands <| {game with Player = p}

            | _, SetChannelSocketId g ->
                let updatedPlayer = {game.Player with SocketId = g} //Will get the playerId from the Server in due course
                let updatedGame = {game with Player = updatedPlayer}
                let cmd1 = Cmd.ofMsg (("Channel Socket Id is " + string g) |> (Simple >> WriteToConsole))
                let cmds = match game.Player.ActorName with
                            | ActorName None -> cmd1
                            | _ -> Cmd.batch [cmd1; sendMessageToServer updatedGame (Instruction (UpdatePlayer updatedPlayer))]

                updatedGame, cmds

            | _, ConnectionChange status ->
                    { game with ConnectionState = status }, Cmd.none

            | _, ChangeView v ->
                withoutCommands <| {game with ViewState = v}

            | Running r, KeyPress k ->  //Check to see if it's a digit
                                        match Int32.TryParse(k) with
                                                         //If it's a digit we have to see if it's in the random numbers. If it is then "click" it.
                                            | true, n -> match List.tryFindIndex (fun y -> y = n) (getNumbers r.Numbers) with
                                                            | Some z -> game, Cmd.ofMsg(Instruction (NewClickedNumber({Number = n; ListIndex = z})))
                                                            | _ -> withoutCommands <| game

                                                   //Or have we implemented any other character keystrokes?
                                            | _ -> match k with
                                                    | "c" -> game,Cmd.ofMsg(Instruction ClearNumbers)
                                                    | "a" -> game, Cmd.ofMsg(Instruction SingleAuto)
                                                    | _ -> withoutCommands <| game

            | _, KeyPress _ -> withoutCommands <| game

            | _, CloseEvent _ -> match game.CommunicationMode with
                                    | BroadcastMode.ViaHTTP -> forwardMessageToServer game (SysMsg msg) //Because the server close will not be triggered by the websocket closing
                                    | BroadcastMode.ViaWebSocket -> withoutCommands <| game

//Process PlayerMessages
let handlePlayerMessage (msg:PlayerMessage) (game:Model) =

        match msg.msg with
        | GameData g -> match g with
                        | Fail Killed -> let finishedGame = match game.ModelState with
                                                            | Running state -> {game with ModelState = FinishedGame {FinalScore= state.Points; FailReason = Killed; Culprit = Some msg.sender}}
                                                            | _ -> {game with ModelState = FinishedGame {FinalScore= Score 0; FailReason = Killed; Culprit = Some msg.sender}}
                                         finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by %s" (getPlayerName msg.sender.PlayerName)) |> (Simple >> WriteToConsole))

                        | _ -> withoutCommands <| game

        | _ -> withoutCommands <| game

//Process ConsoleMessages
let printConsoleMessage (msg:ConsoleMessage) (game : Model) =
    match msg with
    | Simple s -> Console.WriteLine s
    | Complex c -> Console.WriteLine c.Text //TODO - Are colours are supported in the browser console? 
    | Error e -> Console.WriteLine e.Message
    withoutCommands game

// defines the initial state and initial command (= side-effect) of the application
let init():Model * Cmd<Msg> = initialModel, Cmd.none

//Top-level update function
let update (msg:Msg) (game:Model) : Model * Cmd<Msg> =

    let messageHandler (msg:Msg) =
        match msg with
        | WriteToConsole w -> printConsoleMessage w
        | Instruction i -> handleInstructions i
        | GameData g -> handleData g
        | PlayerMessage pm -> handlePlayerMessage pm
        | SysMsg s -> handleSystemMessages s

    messageHandler msg game


        



