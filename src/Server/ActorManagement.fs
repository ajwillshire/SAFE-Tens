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




let gamesMaster (mailbox: Actor<Msg>) =

    let gamesMasterPersona = {playerName = setPlayerName "GamesMaster"; playerId = PlayerId None; socketId = SocketID Guid.Empty}

    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    consoleWriter <<! ("The GamesMaster is Alive!!", ConsoleColor.Magenta)

    let rec loop(players:Player list, highScores: ScoreLog list) = actor {
    
        let! playerMessage = mailbox.Receive()

        if overDebug then consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg "PlayerMessage Received!" ConsoleColor.Magenta}

        match playerMessage with

        | PlayerMessage m ->

            match m.msg with

                //The NewPlayer instruction is one for the Gamesmaster to instantiate the player hierarchy of actors
                | Instruction (NewPlayer n) ->  let playerNumber = match players.Length with
                                                                    | 0 -> 1
                                                                    | _ -> (players |> List.choose(fun x -> getPlayerId x.playerId)
                                                                                    |> List.max) + 1

                                                let updatedPlyr = {n with playerId = setPlayerId playerNumber}
                                                let playerName = getSafePlayerName updatedPlyr

                                                consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("Trying to spawn NewPlayer - " + playerName) ConsoleColor.Magenta}

                                                let newPlayerActor = spawn mailbox.Context.System playerName (playerActor updatedPlyr) 
                                                newPlayerActor <! SysMsg (SetPlayerId playerNumber)

                                                let newList = players @ [updatedPlyr]
                                                consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("New player registered - " + playerName + " - " + string playerNumber + " (Number of active players: " + (string newList.Length) + ")") ConsoleColor.Magenta}

                                                return! loop(newList, highScores)

                //This is to remove all of the other players from the Actor system, ultimately via means of a Poison Pill                                
                | Instruction (DeleteAllOtherPlayers p) ->  players
                                                                |> List.filter (fun x -> x.playerId <> p.playerId)
                                                                |> List.iter (fun q -> let player = (select ("user/"+ (getSafePlayerName q)) mailbox.Context.System)
                                                                                       player <!% {sender = p; msg = GameData (Fail HardStop)})

                                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg ("All other players deleted!!") ConsoleColor.Red}
                                                            return! loop ([p],highScores)

                | SysMsg CloseEvent -> consoleWriter <<! ("Close event received", ConsoleColor.Cyan)
                                       return! loop (players, highScores)

                //An actor that has been told to "HardStop" will reply with a KillMeNow instruction
                | Instruction KillMeNow ->  let player = select ("user/"+ (getSafePlayerName m.sender)) mailbox.Context.System
                                            player <!!! PoisonPill.Instance
                                            let newPlayers = players |> List.filter (fun x -> x.playerId <> m.sender.playerId)

                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (getSafePlayerName m.sender + " has been killed.") ConsoleColor.Red}
                                            return! loop (newPlayers, highScores)

                | GameData (HighScore h) -> let newHighScores = ([{playerName = getPlayerName m.sender.playerName; actorName = getSafePlayerName m.sender; highScore = h}] @ highScores)
                                                                |> List.sortByDescending(fun a -> a.highScore)
                                                                |> List.truncate(5)

                                            let player = (select ("user/"+ (getSafePlayerName m.sender)) mailbox.Context.System)
                                            player <!& ScoreLogs newHighScores

                                            consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg (sprintf "Current High Scorer is %s with %i" newHighScores.[0].playerName (getScoreValue newHighScores.[0].highScore)) ConsoleColor.Red}
                                            return! loop (players, newHighScores)

                | WriteToConsole _ -> consoleWriter <!% m


                //All other messages get routed through to the relevant player
                | _ ->  let getPlayer = players |> List.filter(fun x -> x.playerId = m.sender.playerId)
                                                |> List.tryExactlyOne
                    
                        match getPlayer with
                        | Some p -> consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("GamesMaster sending on message to " + string p + string m.msg) ConsoleColor.Blue}
                                    select ("user/"+ getSafePlayerName p)  mailbox.Context.System  <! m.msg
                                    //return! loop(players, highScores)

                        | None ->  consoleWriter <!% {sender = gamesMasterPersona; msg = cnslMsg  ("That player was not found - " + string m.sender + string m.msg) ConsoleColor.DarkRed}

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









