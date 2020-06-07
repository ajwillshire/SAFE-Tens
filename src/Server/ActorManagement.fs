module ActorManagement

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor

open Shared
open TensTypes
open MessageTypes
open Operators
open PlayerActors


let debug = true
let overDebug = false

//Take all of the console messages and deal with them concurrently - it keeps the colours in check!
let consoleWriter (mailbox: Actor<Msg>) =

    let writeMessage (changeText: string -> string) m = match m with
                                                        | Complex c ->  Console.ForegroundColor <- enum<ConsoleColor>(c.colour)
                                                                        Console.WriteLine(changeText c.msg)
                                                                        Console.ResetColor()

                                                        | Simple s ->   Console.WriteLine(changeText s)

                                                        | Error e ->    Console.ForegroundColor <- ConsoleColor.Red
                                                                        Console.WriteLine(e.Message)
                                                                        Console.ResetColor()

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        match message with
            | PlayerMessage pm ->

                match pm.msg with
                    | WriteToConsole m ->   let newText s = s + sprintf " (%s, %i)" (getPlayerName pm.sender.playerName) (match getPlayerId pm.sender.playerId with | Some s -> s | _-> 0)
                                            writeMessage newText m
                    | _ -> ()

            | WriteToConsole m -> writeMessage (fun x -> x) m

            | _ -> ()

        return! loop ()
    }
    loop ()


let getHighScores scoreList = scoreList
                                |> List.sortByDescending(fun a -> a.highScore)
                                |> List.truncate(5)

// Player helpers
let updatePlayer id modifiedPlayer (playerList: Player list) =
    playerList |> List.map (fun x -> if x.playerId = id then modifiedPlayer else x)

let getPlayerById id (playerList: Player list) =
    playerList |> List.filter (fun p -> p.playerId = id) |> List.tryHead

let getPlayerBySocket socketId (playerList: Player list) =
    playerList |> List.filter (fun p -> p.socketId = socketId) |> List.tryHead

let removePlayerById id (playerList: Player list) =
    playerList |> List.filter (fun p -> p.playerId <> id)

let removePlayerBySocket socketId (playerList: Player list) =
    playerList |> List.filter (fun p -> p.socketId <> socketId)

let getActor (mailbox:Actor<_>) (player:Player)  =
    select ("user/" + (player.refName)) mailbox.Context.System

let getSenderActor (mailbox:Actor<_>) (pm:PlayerMessage) =
    getActor mailbox pm.sender

//*********************

let gamesMaster (mailbox: Actor<Msg>) =

    let gamesMasterPersona = makeNewPlayer "GamesMaster" -1 Guid.Empty
    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    consoleWriter <<! ("The GamesMaster is Alive!!", ConsoleColor.Magenta)

    let rec loop(players:Player list, highScores: ScoreLog list) = actor {
    
        let! playerMessage = mailbox.Receive()

        if overDebug then consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg "PlayerMessage Received!" ConsoleColor.Magenta}

        match playerMessage with

        | PlayerMessage m ->

            let mSender = m.sender

            match m.msg with

                //The NewPlayer instruction is one for the Gamesmaster to instantiate the player hierarchy of actors
                | Instruction (NewPlayer) ->    let playerNumber = match players.Length with
                                                                    | 0 -> 1
                                                                    | _ -> (players |> List.choose(fun x -> getPlayerId x.playerId)
                                                                                    |> List.max) + 1

                                                let updatedPlyr = {mSender with playerId = setPlayerId playerNumber; actorName = (setActorName mSender.playerName (PlayerId (Some playerNumber)))}

                                                let playerName = updatedPlyr.refName

                                                consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("Trying to spawn NewPlayer - " + playerName) ConsoleColor.Magenta}

                                                let newPlayerActor = spawn mailbox.Context.System playerName (playerActor updatedPlyr) 
                                                newPlayerActor <! SysMsg (SetPlayerId playerNumber)
                                                newPlayerActor <!& ScoreLogs (getHighScores highScores)

                                                let newList = players @ [updatedPlyr]
                                                consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("New player registered - " + playerName + " - " + string playerNumber + " (Number of active players: " + (string newList.Length) + ")") ConsoleColor.Magenta}

                                                return! loop(newList, highScores)

                | Instruction (AdoptPlayer id) -> let orphanedPlayer = players |> getPlayerById id
                                                  match orphanedPlayer with
                                                    | Some op -> let adoptedPlayer = {op with socketId = m.sender.socketId; playerName = m.sender.playerName; orphaned = false}
                                                                 consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("Player adopted - " + (getPlayerName adoptedPlayer.playerName)) ConsoleColor.Magenta}
                                                                 adoptedPlayer |> getActor mailbox <!! UpdatePlayer adoptedPlayer
                                                                 return! loop(updatePlayer id adoptedPlayer players, highScores)
                                                    | None -> return! loop(players, highScores)

                //This is to remove all of the other players from the Actor system, ultimately via means of a Poison Pill                                
                | Instruction (DeleteAllOtherPlayers p) ->  players
                                                                |> List.filter (fun x -> x.playerId <> p.playerId)
                                                                |> List.iter(fun q -> getActor mailbox q <!% {sender = p; msg = GameData (Fail HardStop)})
                                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg ("All other players deleted!!") ConsoleColor.Red}
                                                            return! loop ([p],highScores)

                | SysMsg (CloseEvent s) -> consoleWriter <<! ("Close event received from " + m.sender.refName, ConsoleColor.Cyan)
                                           
                                           match getPlayerBySocket s players with
                                           | Some p -> let updatedPlayer = {p with orphaned = true} |> List.singleton
                                                       let otherPlayers = players |> List.filter (fun x -> x.socketId <> s)
                                                       let thisActor = p |> getActor mailbox
                                                       [Instruction.StopRandom; Instruction.StopAuto] |> List.iter (fun z -> thisActor <!! z)
                                                       consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (getPlayerName p.playerName + " has been orphaned.") ConsoleColor.Red}
                                                       return! loop (otherPlayers @ updatedPlayer, highScores)

                                           | None -> return! loop (players, highScores)

                //An actor that has been told to "HardStop" will reply with a KillMeNow instruction
                | Instruction KillMeNow ->  let player = getSenderActor mailbox m
                                            player <!!! PoisonPill.Instance
                                            let newPlayers = players |> List.filter (fun x -> x.playerId <> m.sender.playerId)

                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (m.sender.refName + " has been killed.") ConsoleColor.Red}
                                            return! loop (newPlayers, highScores)

                | GameData (HighScore h) -> let newHighScores = getHighScores ([{playerName = getPlayerName m.sender.playerName; actorName = m.sender.refName; highScore = h}] @ highScores)
                                            //Send high scores to all players
                                            players |> List.iter (fun p -> getActor mailbox p <!& ScoreLogs newHighScores)

                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (sprintf "Current High Scorer is %s with %i" newHighScores.[0].playerName (getScoreValue newHighScores.[0].highScore)) ConsoleColor.Red}
                                            return! loop (players, newHighScores)

                | WriteToConsole _ -> consoleWriter <!% m

                | Instruction SendAllPlayers -> //Can't send via actors because may not have been created
                                                consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg ("Sending player data to " + string (getSocketID m.sender.socketId)) ConsoleColor.Red}
                                                //do Channel.sendMessageViaHub (getSocketID m.sender.socketId) (players |> (GameData.Players >> Msg.GameData)) (sprintf "Communications Error") |> ignore
                                                players |> (GameData.Players >> Msg.GameData) |> Channel.sendMessageToPlayerClient m.sender 
                                                return! loop(players, highScores)

                //All other messages get routed through to the relevant player
                | _ ->  match getPlayerById m.sender.playerId players with
                        | Some p -> consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("GamesMaster sending on message to " + string p + string m.msg) ConsoleColor.Blue}
                                    getActor mailbox p  <! m.msg
                                    
                        | None ->  consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("That player was not found - " + string m.sender + string m.msg) ConsoleColor.DarkRed}

                        return! loop(players, highScores)


        | SysMsg (CloseEvent s) -> match getPlayerBySocket s players with
                                   | Some p -> consoleWriter <<! ("Close event received from " + p.refName, ConsoleColor.Cyan)
                                               let updatedPlayer = {p with orphaned = true}
                                               let otherPlayers = players |> List.filter (fun x -> x.socketId <> s)
                                               [Instruction.StopRandom; Instruction.StopAuto] |> List.iter (fun z -> getActor mailbox p <!! z)
                                               consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (getPlayerName p.playerName + " has been orphaned.") ConsoleColor.Red}
                                               return! loop (otherPlayers @ [updatedPlayer], highScores)

                                   | None -> consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("That player was not found") ConsoleColor.DarkRed}
                                             return! loop(players, highScores)

        | _ -> return! loop(players, highScores)
    }
    loop ([],[])


let spawnActors = 

    let actorSystem = System.create "tensSystem" <| Configuration.load()

    let consoleWriter = spawn actorSystem "consoleWriter" consoleWriter
    consoleWriter <! cnslMsg "Called the spawnActors routine" ConsoleColor.DarkCyan

    spawn actorSystem "gamesMaster" gamesMaster |> ignore
    consoleWriter <! cnslMsg "Actors Spawning" ConsoleColor.DarkBlue

    actorSystem









