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
     finalScore:Score
     failReason:FailMessage
     culprit:Player option
    }

type ModelState =
    | NotStarted
    | Running of Running 
    | Finished of GameOver




type Model =
    {
    Player : Player
    ModelState : ModelState
    ExtraData : Extras
    ViewState : ViewState
    }

let private withoutCommands model = model, Cmd.none

let private noPlayer = {socketId = SocketID Guid.Empty; playerId = PlayerId None; playerName = PlayerName None}
let private blankExtras = {HighScore = 0}

let private newGame (model:Model) = {model with ModelState = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = Score 0}} //;

let initialModel = {ModelState = NotStarted; Player = noPlayer; ExtraData = blankExtras; ViewState = SimpleView} //; playerWs = None}

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> = initialModel, Cmd.none

let forwardToServer (game:Model) (msg:Msg) =    let pm = PlayerMessage {msg = msg; plyr = game.Player}
                                                let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", pm)) () //Thing to do
                                                                               (fun(m) -> m)                                    //Message if it succeeds
                                                                               (fun(e) -> (Error >> WriteToConsole) e)          //Message if it fails
                                                game, cmd


// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (game : Model) : Model * Cmd<Msg> =

    match game.ModelState, msg with
       
    | _, Msg.WriteToConsole msg ->
        match msg with
        | Simple s -> Console.WriteLine s
        | Complex c -> Console.WriteLine c.msg //Colours aren't supported (?)
        | Error e -> Console.WriteLine e.Message

        withoutCommands game

    | state, Instruction message -> 
        match state, message with
        
                | NotStarted, UpdatePlayerName s ->
                    withoutCommands <| {game with Player = {game.Player with playerName = setPlayerName(s)}}

                | _, StartGame ->
                    forwardToServer (newGame game) msg

                | _, ChangeView v ->
                    withoutCommands <| {game with ViewState = v}

                | _, NewPlayer p ->
                    Console.WriteLine "Sending NewPlayer command..."
                    forwardToServer game msg

                //All other Running commands get sent to the Server
                | _, _ -> forwardToServer game msg

    | state, GameData message ->
        Console.WriteLine (sprintf "GameData Message Received %s" (string message))
        match state, message with

                | _, SetChannelSocketId g ->
                    let newPlayer = {game.Player with socketId = g} //Will get the playerId from the Server in due course
                    let cmd1 = Cmd.ofMsg (("Channel socket Id is " + string g) |> (Simple >> WriteToConsole))
                    {game with Player = newPlayer}, cmd1 

                | _, SetPlayerId i ->
                    let newGame = {game with Player = {game.Player with playerId = setPlayerId i}}
                    withoutCommands newGame

                | Running state, GameNums ints ->
                    match ints with
                    | RandomNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Numbers = ints}}
                    | ClickedNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Clicked = ints}}

                | Running state, ScoreUpdate p ->
                    Console.WriteLine (sprintf "Score received - %i" (getScoreValue p))
                    withoutCommands <| {game with ModelState = Running {state with Points = p}}

                | Running state, Fail f ->
                    let finishedGame = {game with ModelState = Finished {finalScore= state.Points; failReason = f; culprit = None}}
                    match f with
                    | TooManyNumbers -> finishedGame, Cmd.ofMsg("Too many numbers in random set." |> (Simple >> WriteToConsole))
                    | OverTen -> finishedGame, Cmd.ofMsg("The sum of total clicked numbers exceeded ten." |> (Simple >> WriteToConsole))
                    | HardStop -> finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by someone.") |> (Simple >> WriteToConsole))

                | _ -> withoutCommands <| game

    | Running state, PlayerMessage pm ->    Console.WriteLine "PlayerMessage received! (Probably a fail)"
                                            match pm.msg with
                                            | GameData g -> match g with
                                                            | Fail (HardStop) -> let finishedGame = {game with ModelState = Finished {finalScore= state.Points; failReason = HardStop; culprit = Some pm.plyr}}
                                                                                 finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by %s" (getPlayerName pm.plyr.playerName)) |> (Simple >> WriteToConsole))
                                                            | _ -> withoutCommands <| game
                                            | _ -> withoutCommands <| game




     | _ ->    withoutCommands <| game


