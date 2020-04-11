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
let overDebug = true

////Add an operator to allow sending a PoisonPill
//let private (<!!!) a (b:PoisonPill) = a <! b

////Overload <! to ensure that only a Msg can be sent using <! (except for the exception above!)
//let private (<!) a (b:Msg) = a<!b

////Add a new operator to make it simpler to pass instructions around the place - Msg | Instruction
//let private (<!!) a (b:Instruction) = a <! (Instruction b)

////Add a new operator to make it simpler to pass data around the place - Msg | GameData
//let private (<!&) a (b:GameData) = a <! (GameData b)

////Add a new operator to make it simpler to pass data around the place - Msg | GameData
//let private (<!%) a (b:PlayerMessage) = a <! (PlayerMessage b)

//Helper function to make it easier to send messages to the console
//let private cnslMsg m c = WriteToConsole ({msg = m; colour = int c} |> Complex)



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
                    | WriteToConsole m ->   let newText s = s + sprintf " (%s, %i)" (getPlayerName pm.plyr.playerName) (match getPlayerId pm.plyr.playerId with | Some s -> s | _-> 0)
                                            writeMessage newText m
                    | _ -> ()

            | WriteToConsole m -> writeMessage (fun x -> x) m

            | _ -> ()

        return! loop ()
    }
    loop ()


type ScoreLog = {playerName:string; highScore: Score}

let gamesMaster (mailbox: Actor<Msg>) =

    let gamesMasterPersona = {playerName = setPlayerName "GamesMaster"; playerId = PlayerId None; socketId = SocketID Guid.Empty}

    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    consoleWriter <<! ("The GamesMaster is Alive!!", ConsoleColor.Magenta)

    let rec loop(players:Player list, highScores: ScoreLog list) = actor {
    
        let! playerMessage = mailbox.Receive()

        if overDebug then consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  "PlayerMessage Received!" ConsoleColor.Magenta}

        match playerMessage with

            | PlayerMessage m ->

                match m.msg with

                    //The NewPlayer instruction is one for the Gamesmaster to instantiate the player hierarchy of actors
                    | Instruction (NewPlayer n) -> let playerNumber = match players.Length with
                                                                        | 0 -> 1
                                                                        | _ -> (players |> List.choose(fun x -> getPlayerId x.playerId)
                                                                                        |> List.max) + 1

                                                   let updatedPlyr = {n with playerId = setPlayerId playerNumber}
                                                   let playerName = getSafePlayerName updatedPlyr.playerName

                                                   consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("Trying to spawn NewPlayer - " + playerName) ConsoleColor.Magenta}

                                                   let newPlayerActor = spawn mailbox.Context.System playerName (playerActor updatedPlyr) 
                                                   newPlayerActor <!& (SetPlayerId playerNumber)
                                                   let newList = players @ [updatedPlyr]
                                                   consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("New player registered - " + playerName + " - " + string playerNumber + " (Number of active players: " + (string newList.Length) + ")") ConsoleColor.Magenta}

                                                   return! loop(newList, highScores)

                    //This is to remove all of the other players from the Actor system, ultimately via means of a Poison Pill                                
                    | Instruction (DeleteAllOtherPlayers p) -> players
                                                                 |> List.filter (fun x -> x.playerName <> p.playerName)
                                                                 |> List.iter (fun q -> let player = (select ("user/"+ (getSafePlayerName q.playerName)) mailbox.Context.System)
                                                                                        player <!% {plyr = p; msg = GameData (Fail HardStop)})

                                                               consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg ("All other players deleted!!") ConsoleColor.Red}
                                                               return! loop ([p],highScores)

                    //An actor that has been told to "HardStop" will reply with a KillMeNow instruction
                    | Instruction KillMeNow -> mailbox.Sender() <!!! PoisonPill.Instance
                                               let senderPath = mailbox.Sender().Path.ToString()
                                               consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg (senderPath + " has been killed.") ConsoleColor.Red}
                                               return! loop (players, highScores)

                    | GameData (HighScore h) -> let newHighScores = ([{playerName = getPlayerName m.plyr.playerName; highScore = h}] @ highScores)
                                                                    |> List.sortByDescending(fun a -> a.highScore)
                                                                    |> List.truncate(5)

                                                consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg (sprintf "Current High Scorer is %s with %i" newHighScores.[0].playerName (getScoreValue newHighScores.[0].highScore)) ConsoleColor.Red}
                                                return! loop (players, newHighScores)


                    //All other messages get routed through to the relevant player
                    | _ ->  let getPlayer = players |> List.filter(fun x -> x.playerId = m.plyr.playerId)
                                                    |> List.tryExactlyOne
                    
                            match getPlayer with
                            | Some p -> consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("GamesMaster sending on message to " + string p) ConsoleColor.Blue}
                                        select ("user/"+ getSafePlayerName p.playerName)  mailbox.Context.System  <! m.msg
                                        //return! loop(players, highScores)

                            | None ->  consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("That player was not found - " + string m.plyr) ConsoleColor.DarkRed}

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









