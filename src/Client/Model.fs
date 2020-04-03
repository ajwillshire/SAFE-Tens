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
    Points : int
    GameType: GameType
  }

type ModelState =
    | NotStarted
    | Running of Running 
    | Finished of int

type Extras =
    {
     HighScore : int
    }

type Model =
    {
    Player : Player
    ModelState : ModelState
    ExtraData : Extras
    //playerWs:Option<WebSocket>
    }

let private withoutCommands model = model, Cmd.none


let private noPlayer = {socketId = None; playerId = Some 0; playerName = None}
let private blankExtras = {HighScore = 0}

let private newGame (model:Model) (gameType: GameType) = {model with ModelState = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = 0; GameType = gameType}} //;
                                     //Player = model.Player
                                     //ExtraData = blankExtras
                                     //playerWs = model.playerWs}

let initialModel = {ModelState = NotStarted; Player = noPlayer; ExtraData = blankExtras} //; playerWs = None}

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> = initialModel, Cmd.none

let forwardToServer (game:Model) (msg:Msg) =    let pm = {msg = msg; plyr = game.Player}
                                                let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", pm)) () //Thing to do
                                                                               (fun(m) -> m)                                    //Message if it succeeds
                                                                               (fun(e) -> (Error >> WriteToConsole) e)          //Message if it fails
                                                game, cmd


//let forwardToSocket (game:Model) (msg:Msg) = match game.playerWs with
//                                                | Some s -> s.send (Thoth.Json.Encode.Auto.toString(0, {plyr = game.Player; msg = msg}))
//                                                | None -> Console.WriteLine "No socket connection"
//                                             withoutCommands game

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

                | NotStarted, NewPlayer _ -> forwardToServer (game) msg

                | NotStarted, StartGame t ->
                    //forwardToServer (newGame game) msg
                    forwardToServer (newGame game t) msg

                | Finished _, StartGame t ->
                    //forwardToServer (newGame game) msg
                    forwardToServer (newGame game t) msg                    


                // This command will probably come from the Server upon failure
                | Running state, StopGame ->
                    withoutCommands <| {game with ModelState = Finished state.Points}

                //All other commands get sent to the Server
                | Running _, _ -> forwardToServer game msg

                //| Finished _, RestartGame ->
                //    forwardToServer (newGame game) msg

                | _ -> withoutCommands <| game

    | state, GameData message ->
        Console.WriteLine (sprintf "GameData Message Received %s" (string message))
        match state, message with
                | _, SetChannelSocketId g ->
                    let cmd1 = Cmd.ofMsg (("Channel socket Id is " + string g) |> (Simple >> WriteToConsole))
                    let shortName = "Henry-" + ((string g).[0..4])
                    let newPlayer = {socketId = Some g; playerName = Some shortName; playerId = None} //Will get the playerId from the Server in due course
                    let cmd2 = Cmd.ofMsg(Instruction <| NewPlayer newPlayer)
                    let cmds = Cmd.batch([cmd1; cmd2])
                    {game with Player = newPlayer}, cmds

                //| _, SetWebSocket w ->
                //     let updatedGame = {game with playerWs = Some w}
                //     Console.WriteLine "Websocket stored"
                //     withoutCommands updatedGame

                | _, SetPlayerId i ->
                    let newGame = {game with Player = {game.Player with playerId = Some i}}
                    withoutCommands newGame

                | Running state, GameNums ints ->
                    match ints with
                    | RandomNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Numbers = ints}}
                    | ClickedNumbers _ -> withoutCommands <| {game with ModelState = Running {state with Clicked = ints}}

                | Running state, ScoreUpdate p ->
                    Console.WriteLine (sprintf "Score received - %i" (scoreValue p))
                    withoutCommands <| {game with ModelState = Running {state with Points = scoreValue p}}

                | Running _, Fail f ->
                    match f with
                    | TooManyNumbers -> game, Cmd.ofMsg("Too many numbers in random set." |> (Simple >> WriteToConsole))
                    | OverTen -> game, Cmd.ofMsg("The sum of total clicked numbers exceeded ten." |> (Simple >> WriteToConsole))

                | _ -> withoutCommands <| game


