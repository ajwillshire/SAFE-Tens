module Actors

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor

open Shared
open CommTypes
open TensTypes
open MessageTypes

open AutoPlayer

let debug = true
let overDebug = true



let private random = System.Random()

//Add an operator to allow sending a PoisonPill
let private (<!!!) a (b:PoisonPill) = a <! b

let private (<!%) a (b:PlayerMessage) = a <! b

//Overload <! to ensure that only a Msg can be sent using <! (except for the exceptions above!)
let private (<!) a (b:Msg) = a<!b

//Add a new operator to make it simpler to pass instructions around the place - Msg | Instruction
let private (<!!) a (b:Instruction) = a <! (Instruction b)

//Add a new operator to make it simpler to pass data around the place - Msg | GameData
let private (<!&) a (b:GameData) = a <! (GameData b)



//Helper function to make it easier to send messages to the console
let private cnslMsg m c = WriteToConsole ({msg = m; colour = int c} |> Complex)

//Take all of the console messages and deal with them concurrently - it keeps the colours in check!
let consoleWriter (mailbox: Actor<PlayerMessage>) =

    let rec loop () = actor {
        let! pm = mailbox.Receive ()

        let player = pm.plyr
        let message = pm.msg

        match message with
            | WriteToConsole m ->   let newText s = s + sprintf " (%s, %i)" player.playerName.Value player.playerId.Value

                                    match m with
                                    | Complex c ->  Console.ForegroundColor <- enum<ConsoleColor>(c.colour)
                                                    Console.WriteLine(newText c.msg)
                                                    Console.ResetColor()
                                    | Simple s -> Console.WriteLine(newText s)
                                    | Error e -> Console.ForegroundColor <- ConsoleColor.Red
                                                 Console.WriteLine(e.Message)
                                                 Console.ResetColor()

            | _ -> ()

        return! loop ()
    }
    loop ()


//Used by the scheduler to keep track of Cancelables to cease messages
type LocalCancelables =
    {
        randomCancel:Cancelable
        autoCancel:Cancelable
    }

let scheduler (mailbox: Actor<Msg>) =

    //let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let mailMan = select "../mailMan"  mailbox.Context
    let genAgent = select "../randomGenerator"  mailbox.Context
    let autoPlayer = select "../auto"  mailbox.Context

    mailMan <! cnslMsg "Scheduler created" ConsoleColor.Blue

    let myScheduler = mailbox.Context.System.Scheduler

    let initialCancelables = {randomCancel =new Cancelable(myScheduler); autoCancel = new Cancelable(myScheduler) }

    let rec loop (myCancelables:LocalCancelables) = actor {
        let! message = mailbox.Receive ()

        
        match message with

        | Instruction i -> match i with
                            | StartRandom ->
                                myCancelables.randomCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with randomCancel=new Cancelable(myScheduler)}
       
                                mailMan <! cnslMsg "Send scheduled messages!" ConsoleColor.Red
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.), genAgent, Instruction Poke, ActorRefs.Nobody, newCancelables.randomCancel)
                                return! loop(newCancelables)

                            | StopRandom -> 
                                mailMan <! cnslMsg "Stop scheduled messages!" ConsoleColor.Red
                                myCancelables.randomCancel.Cancel() 
                                return! loop(myCancelables)

                            | StartAuto ->
                                myCancelables.autoCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with autoCancel=new Cancelable(myScheduler)}
       
                                mailMan <! cnslMsg "Start autopick!" ConsoleColor.Magenta
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.5), autoPlayer, Instruction Poke, ActorRefs.Nobody, newCancelables.autoCancel)
                                return! loop(newCancelables)

                            | StopAuto ->
                                mailMan <! cnslMsg "Stop autopick!" ConsoleColor.Magenta
                                myCancelables.autoCancel.Cancel()
                                return! loop(myCancelables)

                            | _ -> return! loop (myCancelables)

        | _ -> return! loop (myCancelables)
        
    }
    loop (initialCancelables)


let randomNumberGenerator (mailbox: Actor<Msg>) =

    //let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let mailMan = select "../mailMan"  mailbox.Context
    let randomHandler = select "../randomHandler"  mailbox.Context

    mailMan <! cnslMsg "Random generator created" ConsoleColor.Blue

    let rec loop () = actor {
        let! message = mailbox.Receive()

        match message with
        | Instruction Poke -> let newNum = random.Next(1,9)
                              if overDebug then mailMan <! cnslMsg (string newNum) ConsoleColor.DarkBlue
                              randomHandler <!& NewRandom newNum
        | _ -> ()
        
        return! loop ()
    }
    loop ()


let randomHandler (mailbox: Actor<Msg>) =

    //let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let mailMan = select "../mailMan"  mailbox.Context
    let validator = select "../validator"  mailbox.Context

    mailMan <! cnslMsg "RandomHandler created" ConsoleColor.Blue

    let rec loop (randomNumbers:GameNumbers) = actor {
        let! msg = mailbox.Receive()

        match msg with

        | Instruction i -> match i with
                            | ClearNumbers -> let newNums = RandomNumbers[]
                                              mailMan <!& GameNums newNums
                                              if overDebug then mailMan <! cnslMsg "Clearing random numbers" ConsoleColor.DarkCyan
                                              return! loop(newNums)

                            | RemoveNumber i -> let newNums = removeFromGameNumbers randomNumbers i
                                                mailMan <!& GameNums newNums
                                                return! loop (newNums)

                            | SendMeNumbers -> mailbox.Sender() <!& GameNums randomNumbers
                                               return! loop(randomNumbers)

                            | _ -> return! loop(randomNumbers)


        | GameData (NewRandom i) -> let newNums = addToGameNumbers randomNumbers i
                                    mailMan <!& GameNums newNums
                                    validator <!& GameNums newNums //Only needs to happen here as the list grows
                                    return! loop (newNums)



        | _ -> return! loop (randomNumbers)

    }
    loop (RandomNumbers[])


let clickedHandler (mailbox: Actor<Msg>) = 

        //let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
        let mailMan = select "../mailMan"  mailbox.Context
        let randomHandler = select "../randomHandler"  mailbox.Context
        let validator = select "../validator"  mailbox.Context

        mailMan <! cnslMsg "ClickedHandler created" ConsoleColor.Blue

        let rec loop (clickedNumbers:GameNumbers) = actor {
            let! msg = mailbox.Receive ()

            match msg with
            | Instruction i -> match i with 
                                    | NewClickedNumber n -> let newNums = addToGameNumbers clickedNumbers n.number
                                                            if debug then mailMan <! cnslMsg (sprintf "You picked %i!" n.number) ConsoleColor.DarkMagenta
                                                            randomHandler <!! RemoveNumber n
                                                            mailMan <!& GameNums newNums
                                                            validator <!& GameNums newNums
                                                            return! loop (newNums)

                                    | ClearNumbers -> let newNums = ClickedNumbers[]
                                                      mailMan <!& GameNums newNums
                                                      if overDebug then mailMan <! cnslMsg "Clearing clicked numbers" ConsoleColor.DarkCyan
                                                      return! loop(newNums)

                                    | _ -> return! loop (clickedNumbers)

            | _ -> return! loop (clickedNumbers)
                
        }
        loop (ClickedNumbers[])

let validator (mailbox: Actor<Msg>) =

    //let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let mailMan = select "../mailMan"  mailbox.Context

    mailMan <! cnslMsg "Validator created" ConsoleColor.Blue

    let rec loop() = actor {

        let! message = mailbox.Receive()

        let clickedHandler = select "../clickedHandler"  mailbox.Context
        let currentGame = select "../currentGame" mailbox.Context

        match message with

        | GameData d -> match d with
                        | GameNums numbers ->
                            let (a,b) = getLengthAndSum numbers
                            match numbers with
                            | ClickedNumbers _ -> match (a,b) with
                                                    | (x, _) when x > 5 -> currentGame <!& Fail TooManyNumbers
                                                    | (_,y) when y > 10 -> currentGame <!& Fail OverTen
                                                    | (_, 10) -> clickedHandler <! Instruction ClearNumbers
                                                                 mailMan <! cnslMsg "You made 10!" ConsoleColor.Blue
                                                                 currentGame <!! IncrementScore 1
                                                    | _ -> ()
                                          
                            | RandomNumbers _ -> match (a,b) with
                                                    | (y,_) when y >  10 -> currentGame <!& Fail TooManyNumbers
                                                    | _ -> ()

                        | _ -> ()

        | _ -> ()
        return! loop()
    }
    loop()


let currentGame (mailbox: Actor<Msg>) =

    //let consoleWriter = select "/user/consoleWriter" mailbox.Context
    let mailMan = select "../mailMan"  mailbox.Context
    let scheduler = select "../scheduler"  mailbox.Context
    let randomHandler = select "../randomHandler"  mailbox.Context 
    let clickedHandler = select "../clickedHandler"  mailbox.Context
    let parentGame = select "../" mailbox.Context

    let rec loop(currentScore:Score) = actor {

        let! message = mailbox.Receive()

        match message with

            | Instruction i ->  if overDebug then mailMan <! cnslMsg (sprintf "Received Instruction - %s" (string i)) ConsoleColor.Green
                                match i with

                                | StartGame ->  mailMan <! cnslMsg "Start Game" ConsoleColor.Green
                                                //randomHandler <!! ClearNumbers
                                                //clickedHandler <!! ClearNumbers
                                                return! loop(Score 0)

                                | ClearNumbers -> randomHandler <!! ClearNumbers
                                                  clickedHandler <!! ClearNumbers
                                                  mailMan <! cnslMsg "Clearing game numbers" ConsoleColor.Cyan
                                                  return! loop(currentScore)

                                | IncrementScore x ->   let newScore = getScoreValue currentScore + x
                                                        if debug then mailMan <! cnslMsg (sprintf "Current score :%i" newScore) ConsoleColor.DarkGreen
                                                        let newGame = Score newScore
                                                        mailMan <!& ScoreUpdate newGame
                                                        return! loop(newGame)

                                | _ -> return! loop(currentScore)


            | GameData g -> if overDebug then mailMan <! cnslMsg (sprintf "Received Data - %s" (string g)) ConsoleColor.Green
                            match g with
                            | Fail f -> if debug then mailMan <! cnslMsg ("Fail message received!! - " + string f) ConsoleColor.Red 
                                        mailMan <!& Fail f
                                        scheduler <!! StopRandom
                                        scheduler <!! StopAuto
                                        parentGame <!& HighScore currentScore
                                        return! loop(currentScore)

                            | _ -> return! loop(currentScore)

            | _ -> return! loop(currentScore)
        
    }
    loop(Score 0)


//Essentially functions as the Router
let mailMan (player:Player) (mailbox: Actor<Msg>) =

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        let consoleWriter = select "/user/consoleWriter" mailbox.Context

        match message with
            //If we receive an Instruction (probably but not necessarily from Client-side) we need to re-route it.
            | Instruction i ->  consoleWriter <!% {plyr = player; msg = cnslMsg (sprintf "%s Instruction received by MailMan" (string i)) ConsoleColor.DarkRed}
                                match i with
                                | StartGame _ -> select "../currentGame"  mailbox.Context <! message
                                | ClearNumbers -> select "../currentGame"  mailbox.Context <! message

                                | StartRandom -> select "../scheduler"  mailbox.Context <! message
                                | StopRandom -> select "../scheduler"  mailbox.Context <! message

                                | NewClickedNumber _ -> select "../clickedHandler"  mailbox.Context <! message

                                | SingleAuto -> select "../auto"  mailbox.Context <! (Instruction Poke)
                                | StartAuto -> select "../scheduler"  mailbox.Context <! message
                                | StopAuto -> select "../scheduler"  mailbox.Context <! message
                                | _ -> ()

            // If someone sends us data then we need to send it client-side as a Msg (except for a "Fail Hardstop" message which also needs to go to the current Game).
            | GameData g -> do Channel.sendMessageViaHub (getSocketID player.socketId.Value) "message" (GameData g) (sprintf "Communications Error %s" (string g)) |> ignore
                            match g with
                                | Fail (HardStop _) -> select "../currentGame"  mailbox.Context <! message
                                | _ -> consoleWriter <!% {plyr = player; msg = cnslMsg (sprintf "%s GameData received by MailMan" (string g)) ConsoleColor.DarkRed}

            | WriteToConsole m -> consoleWriter <!% {plyr = player; msg = WriteToConsole m}

        return! loop ()
    }
    loop ()


let playerActor (playerSpec:Player) (mailbox : Actor<Msg>) =

    let gamesMaster = select "/user/gamesMaster" mailbox.Context

    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    consoleWriter <! cnslMsg (sprintf "New player named %s has been created!" playerSpec.playerName.Value) ConsoleColor.Green

    let mailMan = spawn mailbox.Context "mailMan" <| mailMan playerSpec
    spawn mailbox.Context "randomHandler" randomHandler  |> ignore
    spawn mailbox.Context "scheduler" (scheduler) |> ignore
    spawn mailbox.Context "clickedHandler" clickedHandler |> ignore
    spawn mailbox.Context "randomGenerator" randomNumberGenerator |> ignore
    spawn mailbox.Context "validator" validator |> ignore
    spawn mailbox.Context "currentGame" currentGame |> ignore
    spawn mailbox.Context "auto" automaticPlayer |> ignore

    let rec loop (highScore:Score) = actor {

        let! message = mailbox.Receive()

        match message with
        | GameData (HighScore h) -> if (getScoreValue h) > (getScoreValue highScore) then
                                        mailMan <! cnslMsg (sprintf "New high score of %i" (getScoreValue h)) ConsoleColor.DarkGreen
                                        gamesMaster <!% {plyr = playerSpec; msg = message}
                                        return! loop(h)
                                    else return! loop(highScore)

        | _ -> mailMan <! message
               return! loop(highScore)
        
    }
    loop(Score 0)


type ScoreLog = {playerName:string; highScore: Score}

let gamesMaster (mailbox: Actor<PlayerMessage>) =

    let gamesMasterPersona = {playerName = Some "GamesMaster"; playerId = Some 0; socketId = None}


    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg "The GamesMaster is Alive!!" ConsoleColor.Magenta}

    let rec loop(players:Player list, highScores: ScoreLog list) = actor {
    
        let! message = mailbox.Receive()

        if overDebug then consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  "PlayerMessage Received!" ConsoleColor.Magenta}

        match message.msg with

            //The NewPlayer instruction is one for the Gamesmaster to instantiate the player hierarchy of actors
            | Instruction (NewPlayer n) -> let playerNumber = match players.Length with
                                                                | 0 -> 1
                                                                | _ -> (players |> List.map(fun x -> x.playerId.Value)
                                                                                |> List.max) + 1

                                           let updatedPlyr = {n with playerId = Some playerNumber}
                                           let playerName = match updatedPlyr.playerName with
                                                                    | Some s -> s
                                                                    | None -> ""

                                           consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("Trying to spawn NewPlayer - " + playerName) ConsoleColor.Magenta}

                                           let newPlayerActor = spawn mailbox.Context.System playerName (playerActor updatedPlyr) 
                                           newPlayerActor <!& (SetPlayerId playerNumber)
                                           let newList = players @ [updatedPlyr]
                                           consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("New player registered - " + playerName + " - " + string playerNumber + " (Number of active players: " + (string newList.Length) + ")") ConsoleColor.Magenta}

                                           return! loop(newList, highScores)

            //This is to remove all of the other players from the Actor system via means of a Poison Pill                                
            | Instruction (DeleteAllOtherPlayers p) -> players
                                                         |> List.filter (fun x -> x.playerName <> p.playerName)
                                                         |> List.iter (fun q -> let player = (select ("user/"+q.playerName.Value) mailbox.Context.System)
                                                                                player <!& (Fail (HardStop p))
                                                                                player <!!! PoisonPill.Instance)
                                                       consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg ("All other players deleted!!") ConsoleColor.Red}
                                                       return! loop ([p],highScores)

            | GameData (HighScore h) -> let newHighScores = ([{playerName = message.plyr.playerName.Value; highScore = h}] @ highScores)
                                                            |> List.sortByDescending(fun a -> a.highScore)
                                                            |> List.truncate(5)

                                        consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg (sprintf "Current High Scorer is %s with %i" newHighScores.[0].playerName (getScoreValue newHighScores.[0].highScore)) ConsoleColor.Red}
                                        return! loop (players, newHighScores)


            //All other messages get sent to the relevant player
            | _ ->  let getPlayer = players |> List.filter(fun x -> x.playerId = message.plyr.playerId)
                                            |> List.tryExactlyOne
                    
                    match getPlayer with
                    | Some p -> consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("GamesMaster sending on message to " + string p) ConsoleColor.Blue}
                                select ("user/"+p.playerName.Value)  mailbox.Context.System  <! message.msg
                                return! loop(players, highScores)

                    | _ ->  consoleWriter <!% {plyr = gamesMasterPersona; msg = cnslMsg  ("That player was not found - " + string message.plyr) ConsoleColor.DarkRed}
                            return! loop(players, highScores)

    }
    loop ([],[])


let spawnActors = 

    let actorSystem = System.create "tensSystem" <| Configuration.load()

    let consoleWriter = spawn actorSystem "consoleWriter" consoleWriter
    consoleWriter <! cnslMsg "Called the spawnActors routine" ConsoleColor.DarkCyan

    spawn actorSystem "gamesMaster" gamesMaster |> ignore
    consoleWriter <! cnslMsg "Actors Spawning" ConsoleColor.DarkBlue

    actorSystem









