module PlayerActors

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor

open Shared
open CommTypes
open TensTypes
open MessageTypes
open Operators

open AutoPlayer

let debug = true
let overDebug = false

let private random = System.Random()

//Used by the scheduler to keep track of Cancelables to cease messages
type private LocalCancelables =
    {
        randomCancel:Cancelable
        autoCancel:Cancelable
    }

let scheduler (mailbox: Actor<Msg>) =

    let mailMan = select "../mailMan"  mailbox.Context
    let genAgent = select "../randomGenerator"  mailbox.Context
    let autoPlayer = select "../auto"  mailbox.Context

    mailMan <<! ("Scheduler created", ConsoleColor.Blue)

    let myScheduler = mailbox.Context.System.Scheduler

    let initialCancelables = {randomCancel =new Cancelable(myScheduler); autoCancel = new Cancelable(myScheduler) }

    let rec loop (myCancelables:LocalCancelables) = actor {
        let! message = mailbox.Receive ()

        
        match message with

        | Instruction i -> match i with
                            | StartRandom ->
                                myCancelables.randomCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with randomCancel=new Cancelable(myScheduler)}
       
                                mailMan <<! ("Send scheduled messages!", ConsoleColor.Red)
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.), genAgent, Instruction Poke, ActorRefs.Nobody, newCancelables.randomCancel)
                                return! loop(newCancelables)

                            | StopRandom -> 
                                mailMan <<! ("Stop scheduled messages!", ConsoleColor.Red)
                                myCancelables.randomCancel.Cancel() 
                                return! loop(myCancelables)

                            | StartAuto ->
                                myCancelables.autoCancel.Cancel() //Hit cancel in case there's one already running
                                let newCancelables = {myCancelables with autoCancel=new Cancelable(myScheduler)}
       
                                mailMan <<! ("Start autopick!", ConsoleColor.Magenta)
                                myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.5), autoPlayer, Instruction Poke, ActorRefs.Nobody, newCancelables.autoCancel)
                                return! loop(newCancelables)

                            | StopAuto ->
                                mailMan <<! ("Stop autopick!", ConsoleColor.Magenta)
                                myCancelables.autoCancel.Cancel()
                                return! loop(myCancelables)

                            | _ -> return! loop (myCancelables)

        | _ -> return! loop (myCancelables)
        
    }
    loop (initialCancelables)


let randomNumberGenerator (mailbox: Actor<Msg>) =

    let mailMan = select "../mailMan"  mailbox.Context
    let randomHandler = select "../randomHandler"  mailbox.Context

    mailMan <<! ("Random generator created", ConsoleColor.Blue)

    let rec loop () = actor {
        let! message = mailbox.Receive()

        match message with
        | Instruction Poke -> let newNum = random.Next(1,9)
                              if overDebug then mailMan <<! (string newNum, ConsoleColor.DarkBlue)
                              randomHandler <!& NewRandom newNum
        | _ -> ()
        
        return! loop ()
    }
    loop ()


let randomHandler (mailbox: Actor<Msg>) =

    let mailMan = select "../mailMan"  mailbox.Context
    let validator = select "../validator"  mailbox.Context

    mailMan <<! ("RandomHandler created", ConsoleColor.Blue)

    let rec loop (randomNumbers:GameNumbers) = actor {
        let! msg = mailbox.Receive()

        match msg with

        | Instruction i -> match i with
                            | ClearNumbers -> let newNums = RandomNumbers[]
                                              mailMan <!& GameNums newNums
                                              if overDebug then mailMan <<! ("Clearing random numbers", ConsoleColor.DarkCyan)
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

        let mailMan = select "../mailMan"  mailbox.Context
        let randomHandler = select "../randomHandler"  mailbox.Context
        let validator = select "../validator"  mailbox.Context

        mailMan <<! ("ClickedHandler created", ConsoleColor.Blue)

        let rec loop (clickedNumbers:GameNumbers) = actor {
            let! msg = mailbox.Receive ()

            match msg with
            | Instruction i -> match i with 
                                    | NewClickedNumber n -> let newNums = addToGameNumbers clickedNumbers n.number
                                                            if debug then mailMan <<! ((sprintf "You picked %i!" n.number), ConsoleColor.DarkMagenta)
                                                            randomHandler <!! RemoveNumber n
                                                            mailMan <!& GameNums newNums
                                                            validator <!& GameNums newNums
                                                            return! loop (newNums)

                                    | ClearNumbers -> let newNums = ClickedNumbers[]
                                                      mailMan <!& GameNums newNums
                                                      if overDebug then mailMan <<! ("Clearing clicked numbers", ConsoleColor.DarkCyan)
                                                      return! loop(newNums)

                                    | _ -> return! loop (clickedNumbers)

            | _ -> return! loop (clickedNumbers)
                
        }
        loop (ClickedNumbers[])

let validator (mailbox: Actor<Msg>) =

    let mailMan = select "../mailMan"  mailbox.Context

    mailMan <<! ("Validator created", ConsoleColor.Blue)

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
                                                                 mailMan <<! ("You made 10!", ConsoleColor.Blue)
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

    let mailMan = select "../mailMan"  mailbox.Context
    let scheduler = select "../scheduler"  mailbox.Context
    let randomHandler = select "../randomHandler"  mailbox.Context 
    let clickedHandler = select "../clickedHandler"  mailbox.Context
    let myPlayer = select "../" mailbox.Context

    let rec loop(currentScore:Score) = actor {

        let! message = mailbox.Receive()

        match message with

            | Instruction i ->  if overDebug then mailMan <<! ((sprintf "Received Instruction - %s" (string i)), ConsoleColor.Green)
                                match i with

                                | StartGame ->  mailMan <<! ("Start Game", ConsoleColor.Green)
                                                mailMan <!& ScoreUpdate (Score 0)
                                                return! loop(Score 0)

                                | ClearNumbers -> randomHandler <!! ClearNumbers
                                                  clickedHandler <!! ClearNumbers
                                                  mailMan <<! ("Clearing game numbers", ConsoleColor.Cyan)
                                                  return! loop(currentScore)

                                | IncrementScore x ->   let incrementedScore = getScoreValue currentScore + x
                                                        if debug then mailMan <<! ((sprintf "Current score :%i" incrementedScore), ConsoleColor.DarkGreen)
                                                        let newScore = Score incrementedScore
                                                        mailMan <!& ScoreUpdate newScore
                                                        return! loop(newScore)

                                | _ -> return! loop(currentScore)


            | GameData g -> if overDebug then mailMan <<! ((sprintf "Received Data - %s" (string g)), ConsoleColor.Green)
                            match g with
                            | Fail f -> if debug then mailMan <<! (("Fail message received!! - " + string f), ConsoleColor.Red)
                                        myPlayer <!& HighScore currentScore
                                        mailMan <!& Fail f
                                        scheduler <!! StopRandom
                                        scheduler <!! StopAuto

                                        if f = HardStop then myPlayer <!! KillMeNow //Changed from mailman...

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
            | Instruction i ->  consoleWriter <!% {sender = player; msg = cnslMsg (sprintf "%s Instruction received by MailMan" (string i)) ConsoleColor.DarkRed}
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

            // If someone sends us data then we need to send it client-side as a Msg.
            | GameData g -> do Channel.sendMessageViaHub (getOptionSocketID player.socketId) (GameData g) (sprintf "Communications Error %s" (string g)) |> ignore
                            consoleWriter <!% {sender = player; msg = cnslMsg (sprintf "%s GameData received by MailMan" (string g)) ConsoleColor.DarkRed}

            | WriteToConsole m -> consoleWriter <!% {sender = player; msg = WriteToConsole m}

            | PlayerMessage pm ->   do Channel.sendMessageViaHub (getOptionSocketID player.socketId) message (sprintf "Communications Error %s" (string pm)) |> ignore
                                    consoleWriter <!% {sender = player; msg = cnslMsg (sprintf "MailMan has received message from %s" (getPlayerName pm.sender.playerName)) ConsoleColor.Blue}
                                    match pm.msg with
                                    | GameData g -> match g with
                                                    | Fail (HardStop) -> select "../currentGame"  mailbox.Context <!& g
                                                    | _ -> ()
                                    | _ -> ()

            // If someone sends us data then we need to send it client-side as a Msg.
            | SysMsg s -> do Channel.sendMessageViaHub (getOptionSocketID player.socketId) (SysMsg s) (sprintf "Communications Error %s" (string s)) |> ignore
                          consoleWriter <!% {sender = player; msg = cnslMsg (sprintf "%s SysData received by MailMan" (string s)) ConsoleColor.DarkRed}

        return! loop ()
    }
    loop ()


let playerActor (playerSpec:Player) (mailbox : Actor<Msg>) =

    let gamesMaster = select "/user/gamesMaster" mailbox.Context
    let consoleWriter = select "/user/consoleWriter" mailbox.Context

    consoleWriter <! cnslMsg (sprintf "New player named %s has been created!" (getPlayerName playerSpec.playerName)) ConsoleColor.Green

    //Create the local hierarchy of Actors to fulfil the game functions
    //These are all child Actors of this actor
    let mailMan = spawn mailbox.Context "mailMan" <| mailMan playerSpec
    spawn mailbox.Context "randomHandler" randomHandler  |> ignore
    spawn mailbox.Context "scheduler" (scheduler) |> ignore
    spawn mailbox.Context "clickedHandler" clickedHandler |> ignore
    spawn mailbox.Context "randomGenerator" randomNumberGenerator |> ignore
    spawn mailbox.Context "validator" validator |> ignore
    spawn mailbox.Context "currentGame" currentGame |> ignore
    spawn mailbox.Context "auto" automaticPlayer |> ignore

    //Keep track of player's high score for the session
    let rec loop (highScore:Score) = actor {

        let! message = mailbox.Receive()

        //Receive a submission from a finished game to update the high score if applicable
        //Also forwards to the GamesMaster
        match message with
        | GameData (HighScore h) -> if (getScoreValue h) > (getScoreValue highScore) then
                                        mailMan <! cnslMsg (sprintf "New high score of %i" (getScoreValue h)) ConsoleColor.DarkGreen
                                        gamesMaster <!% {sender = playerSpec; msg = message}
                                        return! loop(h)
                                    else return! loop(highScore)

        //Anything else is forwarded on to the MailMan
        | _ -> mailMan <! message
               return! loop(highScore)
        
    }
    loop(Score 0)