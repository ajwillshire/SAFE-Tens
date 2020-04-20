module Model

open Elmish
open Thoth.Fetch
open System

//Solution References
open Shared
open CommTypes
open TensTypes
open MessageTypes


//These types are purely local to the Elmish part of the Application
type Running =
  {
    Clicked : GameNumbers
    Numbers : GameNumbers
    Points : Score
  }

type GameOver =
    {
     finalScore : Score
     failReason : FailMessage
     culprit : Player option
    }

type ModelState =
    | NotStarted
    | Running of Running 
    | FinishedGame of GameOver

type Model =
    {
    Player : Player
    ModelState : ModelState
    GameSystemData : GameSystemData
    ViewState : ViewState
    ConnectionState : ConnectionState 
    }



let private noPlayer = {socketId = SocketID Guid.Empty; playerId = PlayerId None; playerName = PlayerName None}
let private blankExtras = {PlayerHighScore = Score 0; SystemHighScores = []}

let private newGame (model:Model) = {model with ModelState = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = Score 0}} //;

let initialModel = {ModelState = NotStarted; Player = noPlayer; GameSystemData = blankExtras; ViewState = SimpleView; ConnectionState = DisconnectedFromServer}

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> = initialModel, Cmd.none

let private withoutCommands model = model, Cmd.none

//let sendPMToServerX(game:Model) (msg:Msg) = let pm = PlayerMessage {msg = msg; sender = game.Player}
//                                            let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", pm)) () //Thing to do
//                                                                          (fun(m) -> m)                                    //Message if it succeeds
//                                                                          (fun(e) -> (Error >> WriteToConsole) e)          //Message if it fails
//                                            cmd

//let forwardToServerX (game:Model) (msg:Msg) = game, sendPMToServerX game msg


//Trying to implement with websockets *******************************************************

let sendPMToServer(game:Model) (msg:Msg) =    let pm = PlayerMessage {msg = msg; sender = game.Player}
                                              match game.ConnectionState with
                                                            | ConnectedToServer s -> s pm |> ignore
                                                            | _ -> ()
                                              Cmd.none

let forwardToServer (game:Model) (msg:Msg) = game, sendPMToServer game msg


// ************************************************************


// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.

let update (msg : Msg) (game : Model) : Model * Cmd<Msg> =

    match game.ModelState, msg with
       
    | _, WriteToConsole msg ->
        match msg with
        | Simple s -> Console.WriteLine s
        | Complex c -> Console.WriteLine c.msg //Colours aren't supported (?)
        | Error e -> Console.WriteLine e.Message

        withoutCommands game

    | state, Instruction message -> 
        match state, message with

                | NotStarted, UpdatePlayerName s ->
                    withoutCommands <| {game with Player = {game.Player with playerName = setPlayerName(s)}}

                | NotStarted, NewPlayer _ ->
                    Console.WriteLine "Sending NewPlayer command..."
                    forwardToServer game msg

                //State can be either NotStarted or FinishedGame
                | _, StartGame ->
                    forwardToServer (newGame game) msg

                //This is more unusual because we need to use the existing playerId to rout the message correctly while replacing it with a new blank Id.
                //Note that the socketId remains constant.
                | _, KillMeNow ->   let cmd1 = sendPMToServer game msg
                                    let cmd2 = Cmd.ofMsg(Instruction ReRegister)
                                    game, Cmd.batch[cmd1;cmd2]

                | _, ReRegister -> {game with ModelState = NotStarted;
                                                            Player = {game.Player with playerId = PlayerId None;
                                                                                       playerName = PlayerName None}}, Cmd.none



                //All other commands get sent to the Server
                | _, _ -> forwardToServer game msg

    | state, GameData message ->
        Console.WriteLine (sprintf "GameData Message Received %s" (string message))
        match state, message with

                | Running state, GameNums ints ->
                    match ints with
                    | RandomNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Numbers = ints}}
                    | ClickedNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Clicked = ints}}

                | Running state, ScoreUpdate p ->
                    Console.WriteLine (sprintf "Score received - %i" (getScoreValue p))
                    withoutCommands <| {game with ModelState = Running {state with Points = p}}

                | _, HighScore h -> withoutCommands <| {game with GameSystemData = {game.GameSystemData with PlayerHighScore = h}}

                | _, ScoreLogs l -> withoutCommands <| {game with GameSystemData = {game.GameSystemData with SystemHighScores = l}}

                | Running state, Fail f ->
                    let finishedGame = {game with ModelState = FinishedGame {finalScore= state.Points; failReason = f; culprit = None}}
                    match f with
                    | TooManyNumbers -> finishedGame, Cmd.ofMsg("Too many numbers in random set." |> (Simple >> WriteToConsole))
                    | OverTen -> finishedGame, Cmd.ofMsg("The sum of total clicked numbers exceeded ten." |> (Simple >> WriteToConsole))
                    | HardStop -> finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by someone.") |> (Simple >> WriteToConsole))
                    | Ended -> finishedGame, Cmd.ofMsg((sprintf "You quit!") |> (Simple >> WriteToConsole))

                | _ -> withoutCommands <| game


    | Running state, PlayerMessage pm ->    Console.WriteLine (sprintf "PlayerMessage received from %s!" (getPlayerName pm.sender.playerName))

                                            match pm.msg with
                                            | GameData g -> match g with
                                                            | Fail (HardStop) -> let finishedGame = {game with ModelState = FinishedGame {finalScore= state.Points; failReason = HardStop; culprit = Some pm.sender}}
                                                                                 finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by %s" (getPlayerName pm.sender.playerName)) |> (Simple >> WriteToConsole))

                                                            | _ -> withoutCommands <| game

                                            | _ -> withoutCommands <| game

    | state, SysMsg s ->
        match state, s with
                | _, SetChannelSocketId g ->
                    let newPlayer = {game.Player with socketId = g} //Will get the playerId from the Server in due course
                    let cmd1 = Cmd.ofMsg (("Channel socket Id is " + string g) |> (Simple >> WriteToConsole))
                    {game with Player = newPlayer}, cmd1

                | _, ConnectionChange status ->
                        { game with ConnectionState = status }, Cmd.none

                | _, SetPlayerId i ->
                    let newGame = {game with Player = {game.Player with playerId = setPlayerId i}}
                    withoutCommands newGame

                | _, ChangeView v ->
                    withoutCommands <| {game with ViewState = v}

                | Running r, KeyPress k ->  //Check to see if it's a digit
                                            match Int32.TryParse(k) with
                                                             //If it's a digit we have to see if it's in the random numbers. If it is then "click" it.
                                                | true, n -> match List.tryFindIndex (fun y -> y = n) (getNumbers r.Numbers) with
                                                                | Some z -> game, Cmd.ofMsg(Instruction (NewClickedNumber({number = n; listIndex = z})))
                                                                | _ -> withoutCommands <| game

                                                       //Or have we implemented any other character keystrokes?
                                                | _ -> match k with
                                                        | "c" -> game,Cmd.ofMsg(Instruction ClearNumbers)
                                                        | "a" -> game, Cmd.ofMsg(Instruction SingleAuto)
                                                        | _ -> withoutCommands <| game

                | _, KeyPress _ -> withoutCommands <| game

                | _, CloseEvent -> forwardToServer game msg


    | _ ->    withoutCommands <| game


