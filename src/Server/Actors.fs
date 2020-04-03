module Actors

open System
open FSharp.Control.Tasks.V2
open Microsoft.FSharp.Core.Operators
open Microsoft.AspNetCore.Http
open Giraffe
open Akka.FSharp
open Akka.Actor

open Shared
open CommTypes
open TensTypes
open MessageTypes
open Saturn

let debug = true
let overDebug = true



let private random = System.Random()

//Overload <! to ensure that only a Msg can be sent using <!
//let private (<!) a (b:Msg) = a<!b

//Add a new operator to make it simpler to pass instructions around the place - Msg | Instruction
let private (<!!) a (b:Instruction) = a <! (Instruction b)

//Add a new operator to make it simpler to pass data around the place - Msg | GameData
let private (<!&) a (b:GameData) = a <! (GameData b)

//This is the creation of the system - needs to be up here as it's referred to a lot!
//let tensSystem = System.create "tensSystem" <| Configuration.load ()

//Helper function to make it easier to send messages to the console
let cnslMsg m c = WriteToConsole ({msg = m; colour = int c} |> Complex)

//Take all of the console messages and deal with them concurrently - it keeps the colours in check!
let consoleWriter (mailbox: Actor<Msg>) =

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        match message with
            | WriteToConsole m -> match m with
                                    | Complex c ->  Console.ForegroundColor <- enum<ConsoleColor>(c.colour)
                                                    Console.WriteLine(c.msg)
                                                    Console.ResetColor()
                                    | Simple s -> Console.WriteLine(s)
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

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let genAgent = select "../randomGenerator"  mailbox.Context
    let autoPlayer = select "../auto"  mailbox.Context

    consoleWriter <! cnslMsg "Scheduler created" ConsoleColor.Blue

    let myScheduler = mailbox.Context.System.Scheduler

    let initialCancelables = {randomCancel =new Cancelable(myScheduler); autoCancel = new Cancelable(myScheduler) }

    let rec loop (myCancelables:LocalCancelables) = actor {
        let! message = mailbox.Receive ()

        
        match message with

        | Instruction i -> match i with
                            | StartRandom ->
                                myCancelables.randomCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with randomCancel=new Cancelable(myScheduler)}
       
                                consoleWriter <! cnslMsg "Send scheduled messages!" ConsoleColor.Red
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.), genAgent, Instruction Poke, ActorRefs.Nobody, newCancelables.randomCancel)
                                return! loop(newCancelables)

                            | StopRandom -> 
                                consoleWriter <! cnslMsg "Stop scheduled messages!" ConsoleColor.Red
                                myCancelables.randomCancel.Cancel() 
                                return! loop(myCancelables)

                            | StartAuto ->
                                myCancelables.autoCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with autoCancel=new Cancelable(myScheduler)}
       
                                consoleWriter <! cnslMsg "Start autopick!" ConsoleColor.Magenta
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.5), autoPlayer, Instruction Poke, ActorRefs.Nobody, newCancelables.autoCancel)
                                return! loop(newCancelables)

                            | StopAuto ->
                                consoleWriter <! cnslMsg "Stop autopick!" ConsoleColor.Magenta
                                myCancelables.autoCancel.Cancel()
                                return! loop(myCancelables)

                            | _ -> return! loop (myCancelables)

        | _ -> return! loop (myCancelables)
        
    }
    loop (initialCancelables)


let randomNumberGenerator (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let randomHandler = select "../randomHandler"  mailbox.Context

    consoleWriter <! cnslMsg "Random generator created" ConsoleColor.Blue

    let rec loop () = actor {
        let! message = mailbox.Receive()

        match message with
        | Instruction Poke -> let newNum = random.Next(1,9)
                              if overDebug then consoleWriter <! cnslMsg (string newNum) ConsoleColor.DarkBlue
                              randomHandler <!& NewRandom newNum
        | _ -> ()
        
        return! loop ()
    }
    loop ()


let randomHandler (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let communicator = select "../communicator"  mailbox.Context
    let validator = select "../validator"  mailbox.Context

    consoleWriter <! cnslMsg "RandomHandler created" ConsoleColor.Blue

    let rec loop (randomNumbers:GameNumbers) = actor {
        let! msg = mailbox.Receive()

        match msg with

        | Instruction i -> match i with
                            | ClearNumbers -> let newNums = RandomNumbers[]
                                              communicator <!& GameNums newNums
                                              if overDebug then consoleWriter <! cnslMsg "Clearing random numbers" ConsoleColor.DarkCyan
                                              return! loop(newNums)

                            | RemoveNumber i -> let newNums = removeFromGameNumbers randomNumbers i
                                                communicator <!& GameNums newNums
                                                return! loop (newNums)

                            | SendMeNumbers -> mailbox.Sender() <!& GameNums randomNumbers
                                               return! loop(randomNumbers)

                            | _ -> return! loop(randomNumbers)


        | GameData (NewRandom i) -> let newNums = addToGameNumbers randomNumbers i
                                    communicator <!& GameNums newNums
                                    validator <!& GameNums newNums //Only needs to happen here as the list grows
                                    return! loop (newNums)



        | _ -> return! loop (randomNumbers)

    }
    loop (RandomNumbers[])


let clickedHandler (mailbox: Actor<Msg>) = 

        let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
        let communicator = select "../communicator"  mailbox.Context
        let randomHandler = select "../randomHandler"  mailbox.Context
        let validator = select "../validator"  mailbox.Context

        consoleWriter <! cnslMsg "ClickedHandler created" ConsoleColor.Blue

        let rec loop (clickedNumbers:GameNumbers) = actor {
            let! msg = mailbox.Receive ()

            match msg with
            | Instruction i -> match i with 
                                    | NewClickedNumber n -> let newNums = addToGameNumbers clickedNumbers n.number
                                                            if debug then consoleWriter <! cnslMsg (sprintf "You picked %i!" n.number) ConsoleColor.DarkMagenta
                                                            randomHandler <!! RemoveNumber n
                                                            communicator <!& GameNums newNums
                                                            validator <!& GameNums newNums
                                                            return! loop (newNums)

                                    | ClearNumbers -> let newNums = ClickedNumbers[]
                                                      communicator <!& GameNums newNums
                                                      if overDebug then consoleWriter <! cnslMsg "Clearing clicked numbers" ConsoleColor.DarkCyan
                                                      return! loop(newNums)

                                    | _ -> return! loop (clickedNumbers)

            | _ -> return! loop (clickedNumbers)
                
        }
        loop (ClickedNumbers[])

let validator (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System

    consoleWriter <! cnslMsg "Validator created" ConsoleColor.Blue

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
                                                                 consoleWriter <! cnslMsg "You made 10!" ConsoleColor.Blue
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

    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    let communicator = select "../communicator"  mailbox.Context
    let scheduler = select "../scheduler"  mailbox.Context
    let randomHandler = select "../randomHandler"  mailbox.Context 
    let clickedHandler = select "../clickedHandler"  mailbox.Context
    let parentGame = select "../" mailbox.Context

    let rec loop(currentScore:Score) = actor {

        let! message = mailbox.Receive()

        match message with

            | Instruction i ->  if overDebug then consoleWriter <! cnslMsg (sprintf "Received Instruction - %s" (string i)) ConsoleColor.Green
                                match i with

                                | StartGame t ->  consoleWriter <! cnslMsg "Start Game" ConsoleColor.Green
                                                  randomHandler <!! ClearNumbers
                                                  clickedHandler <!! ClearNumbers
                                                  return! loop(Score 0)

                                //| RestartGame -> consoleWriter <! cnslMsg "Restart Game" ConsoleColor.Green
                                //                 randomHandler <!! ClearNumbers
                                //                 clickedHandler <!! ClearNumbers
                                //                 return! loop(Score 0)

                                | ClearNumbers -> randomHandler <!! ClearNumbers
                                                  clickedHandler <!! ClearNumbers
                                                  consoleWriter <! cnslMsg "Clearing game numbers" ConsoleColor.Cyan
                                                  return! loop(currentScore)

                                | IncrementScore x ->   let newScore = scoreValue currentScore + x
                                                        if debug then communicator <! cnslMsg (sprintf "Current score :%i" newScore) ConsoleColor.DarkGreen
                                                        let newGame = Score newScore
                                                        communicator <!& ScoreUpdate newGame
                                                        return! loop(newGame)


                                | _ -> return! loop(currentScore)


            | GameData g -> if overDebug then consoleWriter <! cnslMsg (sprintf "Received Data - %s" (string g)) ConsoleColor.Green
                            match g with
                            | Fail f -> if debug then consoleWriter <! cnslMsg ("Fail message received!! - " + string f) ConsoleColor.Red 
                                        communicator <!& Fail f
                                        communicator <!! StopGame //StopGame of Reason?
                                        scheduler <!! StopRandom
                                        scheduler <!! StopAuto
                                        parentGame <!& HighScore currentScore
                                        return! loop(currentScore)

                            | _ -> return! loop(currentScore)

            | _ -> return! loop(currentScore)
        
    }
    loop(Score 0)



type AutoSum = {indices: int list; sum:int}

let getSums (listIn:int list) =

    let pairs = [0..listIn.Length - 2]
                    |> List.map (fun x -> [x+1..listIn.Length-1]
                                                |> List.map (fun y -> {indices = [x; y]; sum = listIn.[x]+listIn.[y]}))

    let triples = [0..listIn.Length - 3]
                    |> List.map (fun x -> [x+1..listIn.Length-2]
                                                |> List.map (fun y -> [y+1..listIn.Length-1]
                                                                            |> List.map (fun z -> {indices = [x; y; z]; sum = listIn.[x]+listIn.[y]+listIn.[z]})))
                                                    
    let yt = [for t in triples do yield! t]
    [for p in (pairs @ yt) do yield! p]
    |> List.filter (fun a -> a.sum = 10)


let automaticPlayer (mailbox: Actor<Msg>) =

    let randomHandler = select "../randomHandler"  mailbox.Context
    let clickedHandler = select "../clickedHandler"  mailbox.Context
    let consoleWriter = select "/user/consoleWriter" mailbox.Context
    let currentGame = select "../currentGame" mailbox.Context

    consoleWriter <! cnslMsg "Automatic player created" ConsoleColor.Blue

    let rec loop() = actor {

        let! message = mailbox.Receive()
        
        match message with

        | Instruction Poke -> randomHandler <!! SendMeNumbers

        | GameData g -> match g with 

                            | GameNums numbers ->
                                match numbers with
                                | ClickedNumbers _ -> ()
                                | RandomNumbers n -> let myPairs = getSums n
                                                     if myPairs.Length > 0 then
                                                        let myIndex = random.Next(0, myPairs.Length-1)
                                                        let myPair = myPairs.[myIndex]

                                                        if overDebug then 
                                                            if myPair.indices.Length = 3 then
                                                                consoleWriter <! cnslMsg (sprintf "Indices: %i %i %i" myPair.indices.[0] myPair.indices.[1] myPair.indices.[2]) ConsoleColor.Magenta
                                                            else
                                                                consoleWriter <! cnslMsg (sprintf "Indices: %i %i" myPair.indices.[0] myPair.indices.[1]) ConsoleColor.Magenta
                                                        
                                                        let myFirstAutoclick = {number = n.[myPair.indices.[0]]; listIndex = myPair.indices.[0]}
                                                        clickedHandler <!! NewClickedNumber myFirstAutoclick
                                                        if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string myFirstAutoclick.number) ConsoleColor.Magenta

                                                        let mySecondAutoclick = {number = n.[myPair.indices.[1]]; listIndex = myPair.indices.[1]-1} //The index is passed on so needs to subtract one, but the number itself isn't
                                                        clickedHandler <!! NewClickedNumber mySecondAutoclick
                                                        if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string mySecondAutoclick.number) ConsoleColor.Magenta

                                                        if myPair.indices.Length = 3 then
                                                            let myThirdAutoclick = {number = n.[myPair.indices.[2]]; listIndex = myPair.indices.[2]-2} //The index is passed on so needs to subtract two, but the number itself isn't
                                                            clickedHandler <!! NewClickedNumber myThirdAutoclick
                                                            if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string myThirdAutoclick.number) ConsoleColor.Magenta

                                                     else
                                                        consoleWriter <! cnslMsg ("Nothing to pick!") ConsoleColor.DarkMagenta
                                                     if n.Length = 9 then
                                                        consoleWriter <! cnslMsg ("Time to cheat... :-D") ConsoleColor.Red
                                                        currentGame <!! ClearNumbers
                            | _ -> ()

        | _ -> ()
        
        return! loop()

    }
    loop()

//Essentially functions as the Router
let mailMan (socketId:Guid) (mailbox: Actor<Msg>) =

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        let consoleWriter = select "/user/consoleWriter" mailbox.Context

        match message with
            //If we receive an Instruction (probably but not necessarily from Client-side) we need to re-route it.
            | Instruction i ->  consoleWriter <! cnslMsg (sprintf "%s Instruction received by MailMan" (string i)) ConsoleColor.DarkRed
                                match i with
                                | StartGame _ -> select "../currentGame"  mailbox.Context <! message
                                //| RestartGame -> select "../currentGame"  mailbox.Context <! message
                                | StopGame -> do Channel.sendMessageViaHub socketId "message" message (sprintf "Communications Error %s" (string i)) |> ignore

                                | StartRandom -> select "../scheduler"  mailbox.Context <! message
                                | StopRandom -> select "../scheduler"  mailbox.Context <! message
                                | ClearNumbers -> select "../currentGame"  mailbox.Context <! message
                                | NewClickedNumber _ -> select "../clickedHandler"  mailbox.Context <! message

                                | SingleAuto -> select "../auto"  mailbox.Context <! (Instruction Poke)
                                | StartAuto -> select "../scheduler"  mailbox.Context <! message
                                | StopAuto -> select "../scheduler"  mailbox.Context <! message
                                | _ -> ()

            // If someone sends us data then we need to send it client-side as a Msg.
            | GameData g -> consoleWriter <! cnslMsg (sprintf "%s GameData received by MailMan" (string g)) ConsoleColor.DarkRed
                            do Channel.sendMessageViaHub socketId "message" (GameData g) (sprintf "Communications Error %s" (string g)) |> ignore

            | WriteToConsole _ -> consoleWriter <! message

        return! loop ()
    }
    loop ()


let playerActor (playerSpec:Player) (mailbox : Actor<Msg>) =

    Console.WriteLine "Player Created"

    let communicator = spawn mailbox.Context "communicator" <| mailMan playerSpec.socketId.Value
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
        | GameData (HighScore h) -> if (scoreValue h) > (scoreValue highScore) then
                                        communicator <! cnslMsg (sprintf "New high score of %i" (scoreValue h)) ConsoleColor.DarkGreen
                                        return! loop(h)
                                    else return! loop(highScore)

        | _ -> communicator <! message
               return! loop(highScore)
        
    }
    loop(Score 0)



let gamesMaster (mailbox: Actor<PlayerMessage>) =
    let consoleWriter = select "/user/consoleWriter" mailbox.Context

    consoleWriter <! cnslMsg "gamesMaster is created!" ConsoleColor.Magenta

    let rec loop(players:Player list) = actor {
    
        let! message = mailbox.Receive()

        if overDebug then consoleWriter <! cnslMsg "PlayerMessage Received!" ConsoleColor.Magenta

        match message.msg with

            | Instruction (NewPlayer n) -> let playerNumber = match players.Length with
                                                                | 0 -> 1
                                                                | _ -> (players |> List.map(fun x -> x.playerId.Value)
                                                                                |> List.max) + 1

                                           let updatedPlyr = {n with playerId = Some playerNumber}
                                           let playerName = updatedPlyr.playerName.Value

                                           consoleWriter <! cnslMsg ("Trying to spawn NewPlayer - " + playerName) ConsoleColor.Magenta

                                           let newPlayerActor = spawn mailbox.Context.System playerName (playerActor message.plyr) //|> ignore
                                           newPlayerActor <!& (SetPlayerId playerNumber)
                                           consoleWriter <! cnslMsg ("New player registered - " + playerName + " - " + string playerNumber) ConsoleColor.Magenta

                                           return! loop(players @ [updatedPlyr])

            | _ ->  let getPlayer = players |> List.filter(fun x -> x.playerId = message.plyr.playerId)
                                            |> List.tryExactlyOne
                    
                    match getPlayer with
                    | Some p -> consoleWriter <! cnslMsg ("GamesMaster sending on message to " + string p) ConsoleColor.Blue
                                select ("user/"+p.playerName.Value)  mailbox.Context.System  <! message.msg
                                return! loop(players)

                    | _ ->  consoleWriter <! cnslMsg ("That player was not found - " + string message.plyr) ConsoleColor.DarkRed
                            return! loop(players)

    }
    loop([])
    


let hubActor (hubIn:Channels.ISocketHub) (mailbox:Actor<PlayerMessage>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context

    consoleWriter <! cnslMsg "hubMaster is Live!!" ConsoleColor.DarkBlue

    let rec loop() = actor {

        let! message = mailbox.Receive()

        let destP = message.plyr
        let msg = message.msg

        hubIn.SendMessageToClient "/channel" destP.socketId.Value "messages" msg |> ignore

        return! loop()

        }
    loop()


let spawnActors = 

    let actorSystem = System.create "tensSystem" <| Configuration.load()

    let consoleWriter = spawn actorSystem "consoleWriter" consoleWriter
    consoleWriter <! cnslMsg "Called the spawnActors routine" ConsoleColor.DarkCyan

    //spawn tensSystem "hubMaster" (hubActor hub) |> ignore

    spawn actorSystem "gamesMaster" gamesMaster |> ignore
    consoleWriter <! cnslMsg "Actors Spawning" ConsoleColor.DarkBlue

    actorSystem









