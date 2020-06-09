module PlayerActors

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor

open Shared.DataTypes
open Shared.MessageTypes

open Operators
open AutoPlayer
open GameActors

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

    let mailMan = getSiblingActor mailbox MailMan
    let genAgent = getSiblingActor mailbox RandomGenerator
    let autoPlayer = getSiblingActor mailbox AutoPlayer

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

    let mailMan = getSiblingActor mailbox MailMan
    let randomHandler = getSiblingActor mailbox RandomHandler

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

    let mailMan = getSiblingActor mailbox MailMan
    let validator = getSiblingActor mailbox Validator

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
                                    validator <!& GameNums newNums
                                    return! loop (newNums)

        | _ -> return! loop (randomNumbers)

    }
    loop (RandomNumbers[])


let clickedHandler (mailbox: Actor<Msg>) = 

        let mailMan = getSiblingActor mailbox MailMan
        let randomHandler = getSiblingActor mailbox RandomHandler
        let validator = getSiblingActor mailbox Validator

        mailMan <<! ("ClickedHandler created", ConsoleColor.Blue)

        let rec loop (clickedNumbers:GameNumbers) = actor {
            let! msg = mailbox.Receive ()

            match msg with
            | Instruction i -> match i with 
                                    | NewClickedNumber n -> let newNums = addToGameNumbers clickedNumbers n.Number
                                                            if debug then mailMan <<! ((sprintf "You picked %i!" n.Number), ConsoleColor.DarkMagenta)
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

    let mailMan = getSiblingActor mailbox MailMan
    let clickedHandler = getSiblingActor mailbox ClickedHandler
    let currentGame = getSiblingActor mailbox CurrentGame

    mailMan <<! ("Validator created", ConsoleColor.Blue)

    let rec loop() = actor {

        let! message = mailbox.Receive()

        match message with

        | GameData d -> match d with
                        | GameNums numbers ->
                            let (a,b) = getLengthAndSum numbers
                            match numbers with
                            | ClickedNumbers _ -> match (a,b) with
                                                    | (x, _) when x > 5 -> currentGame <!& Fail TooManyNumbers
                                                    | (_,y) when y > 10 -> currentGame <!& Fail OverTen
                                                    | (_, 10) -> clickedHandler <!! ClearNumbers
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

    let mailMan = getSiblingActor mailbox MailMan
    let randomHandler = getSiblingActor mailbox RandomHandler 
    let clickedHandler = getSiblingActor mailbox ClickedHandler

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
                                                        if debug then mailMan <<! ((sprintf "Current score: %i" incrementedScore), ConsoleColor.DarkGreen)
                                                        let newScore = Score incrementedScore
                                                        mailMan <!& ScoreUpdate newScore
                                                        return! loop(newScore)

                                | _ -> return! loop(currentScore)


            | GameData g -> if overDebug then mailMan <<! ((sprintf "Received Data - %s" (string g)), ConsoleColor.Green)
                            match g with
                            | Fail f -> if debug then mailMan <<! (("Fail message received!! - " + string f), ConsoleColor.Red)
                                        mailMan <!& HighScore currentScore
                                        mailMan <!! StopRandom
                                        mailMan <!! StopAuto
                                        match f with
                                        | Killed -> mailMan <!! KillMeNow //Will have come from client side
                                        | _ -> mailMan <!& Fail f //Will have come from server side

                            | _ -> return! loop(currentScore)

            | _ -> return! loop(currentScore)
        
    }
    loop(Score 0)


//Essentially functions as the Router for the system
let mailMan (player:Player) (mailbox: Actor<Msg>) =

    let myPlayer = getParentActor mailbox
    let scheduler = getSiblingActor mailbox Scheduler
    let currentGame = getSiblingActor mailbox CurrentGame
    let clickedHandler = getSiblingActor mailbox ClickedHandler
    let autoPlayer = getSiblingActor mailbox AutoPlayer

    let rec loop (player:Player) = actor {
        let! message = mailbox.Receive ()

        let consoleWriter = getSystemActor mailbox ConsoleWriter

        match message with
            | Instruction (UpdatePlayer p) ->   match getPlayerId p.PlayerId with
                                                | Some i -> consoleWriter <<!((sprintf "Player Updated - id %i" i), ConsoleColor.DarkRed)
                                                | None -> consoleWriter <<!((sprintf "Player Updated - No id"), ConsoleColor.Red)

                                                Channel.sendMessageToPlayerClient p (SysMsg (PlayerUpdate p))
                                                return! loop(p)

            | _ -> match message with
                        //If we receive an Instruction (probably but not necessarily from Client-side) we need to re-route it.
                        | Instruction i ->  consoleWriter <<! (sprintf "%s Instruction received by MailMan" (string i), ConsoleColor.DarkRed)
                                            match i with
                                            | StartGame _ -> currentGame <!! i
                                            | ClearNumbers -> currentGame <!! i

                                            | StartRandom -> scheduler <!! i
                                            | StopRandom -> scheduler <!! i

                                            | NewClickedNumber _ -> clickedHandler <!! i

                                            | SingleAuto -> autoPlayer <!! Poke
                                            | StartAuto -> scheduler <!! i
                                            | StopAuto -> scheduler <!! i

                                            | KillMeNow -> myPlayer <!! i

                                            | _ -> ()

                        // If someone sends us data then we need to send it client-side as a Msg.
                        | GameData g -> match g with
                                        | HighScore _ -> myPlayer <!& g    //Send this onto the player for logging
                                        | _ -> Channel.sendMessageToPlayerClient player (GameData g)
                                               consoleWriter <<! (sprintf "%s GameData received by MailMan" (string g), ConsoleColor.DarkRed)

                        | WriteToConsole _ -> consoleWriter <! message

                        | PlayerMessage pm ->   Channel.sendMessageToPlayerClient player message
                                                consoleWriter <<! (sprintf "MailMan has received message from %s" (getPlayerName pm.sender.PlayerName), ConsoleColor.Blue)
                                                match pm.msg with
                                                | GameData g -> match g with
                                                                | Fail (Killed) -> getSiblingActor mailbox CurrentGame <!& g
                                                                | _ -> ()
                                                | _ -> ()

                        // If someone sends us data then we need to send it client-side as a Msg.
                        | SysMsg s -> Channel.sendMessageToPlayerClient player (SysMsg s)
                                      consoleWriter <<! (sprintf "%s SysData received by MailMan" (string s), ConsoleColor.DarkRed)

                   return! loop (player)
    }
    loop (player)

//Assign the correct subroutine to the Actor Type
let getPlayerActorSpec (actor:PlayerActor) (player:Player) =
    match actor with
        | MailMan -> mailMan player
        | RandomHandler -> randomHandler
        | Scheduler -> scheduler
        | ClickedHandler -> clickedHandler
        | RandomGenerator -> randomNumberGenerator
        | Validator -> validator
        | CurrentGame -> currentGame
        | AutoPlayer -> automaticPlayer


//Top-level Actor
let playerActor (player:Player) (mailbox : Actor<Msg>) =

    let gamesMaster = getSystemActor mailbox GamesMaster
    let consoleWriter = getSystemActor mailbox ConsoleWriter
    consoleWriter <<! (sprintf "New player named %s has been created!" (getPlayerName player.PlayerName), ConsoleColor.Green)

    //Create the local hierarchy of Actors to fulfil the game functions
    //These are all child Actors of this actor
    [MailMan; RandomHandler; Scheduler; ClickedHandler; RandomGenerator; Validator; CurrentGame; AutoPlayer]
    |> makePlayerActors mailbox player getPlayerActorSpec

    let mailMan = getChildActor mailbox MailMan

    //Keep track of player's high score for the session
    let rec loop (player:Player) = actor {
        let! message = mailbox.Receive()
        //Keeps track of player High Scores and forwards to the GamesMaster as required
        //Also allows the player to be updated from the Client-side

        match message with
        | GameData (HighScore h) -> if (getScoreValue h) > (getScoreValue player.HighScore) then
                                        let updatedPlayer = {player with HighScore = h}
                                        mailMan <!! UpdatePlayer updatedPlayer
                                        mailMan <<! (sprintf "New high score of %i" (getScoreValue h),ConsoleColor.DarkGreen)
                                        gamesMaster <!% {sender = updatedPlayer; msg = message} //Send the player with the updated high score
                                        return! loop(updatedPlayer)

                                    else gamesMaster <!% {sender = player; msg = message} //Let's keep track of all scores, high or not!
                                         return! loop(player)

        | Instruction (UpdatePlayer p) -> mailMan <! message
                                          mailMan <<! ("PA Updating Player", ConsoleColor.DarkBlue)
                                          return! loop(p)

        | Instruction (KillMeNow) -> gamesMaster <!% {sender = player; msg = message}

        //Anything else is forwarded on to the MailMan to deal with
        | _ -> mailMan <! message
               return! loop(player)
    }
    loop(player)