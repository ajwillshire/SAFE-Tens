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

let private noPlayer = {socketId = None; playerId = None; playerName = None}
let private blankExtras = {HighScore = 0}

let private newGame (model:Model) = {model with ModelState = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = Score 0}} //;

let initialModel = {ModelState = NotStarted; Player = noPlayer; ExtraData = blankExtras; ViewState = SimpleView} //; playerWs = None}

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> = initialModel, Cmd.none

let forwardToServer (game:Model) (msg:Msg) =    let pm = {msg = msg; plyr = game.Player}
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
                    withoutCommands <| {game with Player = {game.Player with playerName = Some s}}

                | _, StartGame ->
                    forwardToServer (newGame game) msg

                | _, ChangeView v ->
                    withoutCommands <| {game with ViewState = v}

                //All other Running commands get sent to the Server
                | _, _ -> forwardToServer game msg

    | state, GameData message ->
        Console.WriteLine (sprintf "GameData Message Received %s" (string message))
        match state, message with

                | _, SetChannelSocketId g ->
                    let newPlayer = {game.Player with socketId = Some g} //Will get the playerId from the Server in due course

                    let cmd1 = Cmd.ofMsg (("Channel socket Id is " + string g) |> (Simple >> WriteToConsole))
                    //let cmd2 = Cmd.ofMsg(Instruction <| NewPlayer newPlayer)
                    {game with Player = newPlayer}, cmd1 //Cmd.batch([cmd1; cmd2])

                | _, SetPlayerId i ->
                    let newGame = {game with Player = {game.Player with playerId = Some i}}
                    withoutCommands newGame

                | Running state, GameNums ints ->
                    match ints with
                    | RandomNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Numbers = ints}}
                    | ClickedNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Clicked = ints}}

                | Running state, ScoreUpdate p ->
                    Console.WriteLine (sprintf "Score received - %i" (getScoreValue p))
                    withoutCommands <| {game with ModelState = Running {state with Points = p}}

                | Running state, Fail f ->
                    let finishedGame = {game with ModelState = Finished {finalScore= state.Points; failReason = f}}
                    match f with
                    | TooManyNumbers -> finishedGame, Cmd.ofMsg("Too many numbers in random set." |> (Simple >> WriteToConsole))
                    | OverTen -> finishedGame, Cmd.ofMsg("The sum of total clicked numbers exceeded ten." |> (Simple >> WriteToConsole))
                    | HardStop p -> finishedGame, Cmd.ofMsg((sprintf "This game was brought to a premature end by %s" p.playerName.Value) |> (Simple >> WriteToConsole))

                | _ -> withoutCommands <| game


