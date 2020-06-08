module SystemActors

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor
open Akka

open Shared
open DataTypes
open MessageTypes
open Operators
open PlayerActors
open GameActors


let debug = true
let overDebug = false

let consoleWriter (mailbox: Actor<Msg>) =

    let writeMessage (changeText: string -> string) m = match m with
                                                        | Complex c ->  Console.ForegroundColor <- enum<ConsoleColor>(c.Colour)
                                                                        Console.WriteLine(changeText c.Text)
                                                                        Console.ResetColor()

                                                        | Simple s ->   Console.WriteLine(changeText s)

                                                        | Error e ->    Console.ForegroundColor <- ConsoleColor.Red
                                                                        Console.WriteLine(e.Message)
                                                                        Console.ResetColor()

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        let sender = mailbox.Sender ()
             


        match message with
            | PlayerMessage pm ->

                match pm.msg with
                    | WriteToConsole m ->   let newText s = s + sprintf " (%s, %i)" (getPlayerName pm.sender.PlayerName) (match getPlayerId pm.sender.PlayerId with | Some s -> s | _-> 0)
                                            writeMessage newText m
                    | _ -> ()

            | WriteToConsole m -> let newText s = s + sprintf " (%s)" (sender.Path.ToStringWithoutAddress())
                                  writeMessage newText m

            | _ -> ()

        return! loop ()
    }
    loop ()




let getHighScores scoreList = scoreList
                                |> List.sortByDescending(fun a -> a.highScore)
                                |> List.truncate(5)


let gamesMaster (mailbox: Actor<Msg>) =

    let consoleWriter = getSystemActor mailbox ConsoleWriter
    consoleWriter <<! ("The GamesMaster is Alive!!", ConsoleColor.Magenta)

    let rec loop(players:PlayerList, highScores: ScoreLog list) = actor {
    
        let! playerMessage = mailbox.Receive()

        match playerMessage with

        | PlayerMessage m ->

            let mSender = m.sender

            match m.msg with

                //The NewPlayer instruction is one for the Gamesmaster to instantiate the player hierarchy of actors
                | Instruction (NewPlayer) ->    let newPlayerId = players.nextId
                                                let updatedPlyr = {mSender with PlayerId = newPlayerId;
                                                                                ActorName = (setActorName mSender.PlayerName newPlayerId)}

                                                let playerName = updatedPlyr.refName

                                                consoleWriter <<! ("Trying to spawn NewPlayer - " + playerName, ConsoleColor.Magenta)

                                                let newPlayerActor = spawn mailbox.Context.System playerName (playerActor updatedPlyr) 
                                                newPlayerActor <! SysMsg (SetPlayerId newPlayerId)
                                                newPlayerActor <!& ScoreLogs (getHighScores highScores)

                                                consoleWriter <<! ("New player registered - " + playerName + " - " + string newPlayerId + " (Number of active players: " + string players.numPlayers + ")", ConsoleColor.Magenta)

                                                return! loop(players.addPlayer updatedPlyr, highScores)

                | Instruction (AdoptPlayer id) -> let orphanedPlayer = players.getPlayerById id
                                                  match orphanedPlayer with
                                                    | Some op -> let adoptedPlayer = {op with SocketId = m.sender.SocketId; PlayerName = m.sender.PlayerName; Orphaned = false}
                                                                 consoleWriter <<! ("Player adopted - " + getPlayerName adoptedPlayer.PlayerName, ConsoleColor.Magenta)
                                                                 adoptedPlayer |> getActor mailbox <!! UpdatePlayer adoptedPlayer
                                                                 return! loop((players.updatePlayer adoptedPlayer), highScores)
                                                    | None -> return! loop(players, highScores)

                //This is to remove all of the other players from the Actor system, ultimately via means of a Poison Pill                                
                | Instruction (DeleteAllOtherPlayers p) ->  players.Players
                                                                |> List.filter (fun x -> x.PlayerId <> p.PlayerId)
                                                                |> List.iter(fun q -> getActor mailbox q <!% {sender = p; msg = GameData (Fail Killed)})
                                                            consoleWriter <<! ("All other players deleted!!", ConsoleColor.Red)
                                                            return! loop ({Players = [p]},highScores)

                | SysMsg (CloseEvent s) -> consoleWriter <<! ("Close event received from " + m.sender.refName, ConsoleColor.Cyan)
                                           
                                           match players.getPlayerBySocket s with
                                           | Some p -> let updatedPlayer = {p with Orphaned = true}
                                                       let thisActor = p |> getActor mailbox
                                                       [Instruction.StopRandom; Instruction.StopAuto] |> List.iter (fun z -> thisActor <!! z)
                                                       consoleWriter <<! (getPlayerName p.PlayerName + " has been orphaned.", ConsoleColor.Red)
                                                       return! loop (players.updatePlayer updatedPlayer, highScores)

                                           | None -> return! loop (players, highScores)

                //An actor that has been told to "HardStop" will reply with a KillMeNow instruction
                | Instruction KillMeNow ->  let player = getSenderActor mailbox m
                                            player <!!! PoisonPill.Instance
                                            consoleWriter <<!(m.sender.refName + " has been killed.", ConsoleColor.Red)
                                            return! loop (players.removePlayerById m.sender.PlayerId, highScores)

                | GameData (HighScore h) -> let newHighScores = getHighScores ([{playerName = getPlayerName mSender.PlayerName; actorName = mSender.refName; highScore = h}] @ highScores)
                                            //Send high scores to all players
                                            players.Players |> List.iter (fun p -> getActor mailbox p <!& ScoreLogs newHighScores)

                                            consoleWriter <<! (sprintf "Current High Scorer is %s with %i" newHighScores.[0].playerName (getScoreValue newHighScores.[0].highScore), ConsoleColor.Red)
                                            return! loop (players.updatePlayer mSender, newHighScores)

                | WriteToConsole _ -> consoleWriter <!% m

                | Instruction SendAllPlayers -> players |> (GameData.Players >> Msg.GameData) |> Channel.sendMessageToPlayerClient m.sender 
                                                consoleWriter <<! ("Sending player data to " + string (getSocketId m.sender.SocketId), ConsoleColor.Red)
                                                return! loop(players, highScores)

                //All other messages get routed through to the relevant player
                | _ ->  match players.getPlayerById m.sender.PlayerId with
                        | Some p -> consoleWriter <<! ("GamesMaster sending on message to " + string p + string m.msg, ConsoleColor.Blue)
                                    getActor mailbox p  <! m.msg
                                    
                        | None ->  consoleWriter <<! ("That player was not found - " + string m.sender + string m.msg, ConsoleColor.DarkRed)

                        return! loop(players, highScores)


        | SysMsg (CloseEvent s) -> match players.getPlayerBySocket s with
                                   | Some p -> consoleWriter <<! ("Close event received from " + p.refName, ConsoleColor.Cyan)
                                               let updatedPlayer = {p with Orphaned = true}
                                               [Instruction.StopRandom; Instruction.StopAuto] |> List.iter (fun z -> getActor mailbox p <!! z)
                                               consoleWriter <<! (getPlayerName p.PlayerName + " has been orphaned.", ConsoleColor.Red)
                                               return! loop (players.updatePlayer updatedPlayer, highScores)

                                   | None -> consoleWriter <<! ("That player was not found", ConsoleColor.DarkRed)
                                             return! loop(players, highScores)

        | _ -> return! loop(players, highScores)
    }
    loop ({Players=[]},[])


let getSystemActorSpec (actor:SystemActor) =
    match actor with
        | GamesMaster -> gamesMaster
        | ConsoleWriter -> consoleWriter


let spawnActors = 

    let actorSystem = System.create "tensSystem" <| Configuration.load()

    [GamesMaster; ConsoleWriter]
    |> makeSystemActors actorSystem getSystemActorSpec

    let consoleWriter = getSystemActor2 actorSystem ConsoleWriter
    consoleWriter <<! ("Called the spawnActors routine", ConsoleColor.DarkCyan)
    consoleWriter <<! ("Actors Spawning", ConsoleColor.DarkBlue)

    actorSystem









