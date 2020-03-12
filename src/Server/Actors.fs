module Actors

open System

open Akka.FSharp
open Router
open Shared
open Akka.Actor
open Microsoft.FSharp.Core.Operators

open CommTypes
open TensTypes
open MessageTypes
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open Giraffe
open System.Threading.Tasks


let debug = true
let overDebug = false

let cnslMsg m c = WriteToConsole ({msg = m; colour = int c} |> Complex)


let private random = System.Random()


let tensSystem = System.create "tensSystem" <| Configuration.load ()

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

            | _ -> ()

        return! loop ()
    }
    loop ()


type LocalCancelables =
    {
        randomCancel:Cancelable
        autoCancel:Cancelable
    }

let scheduler (mailbox: Actor<Msg>) = //genAgent

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let genAgent = select "/user/randomGenerator"  mailbox.Context.System
    let autoPlayer = select "/user/auto"  mailbox.Context.System

    consoleWriter <! cnslMsg "Scheduler created" ConsoleColor.Blue

    let myScheduler = tensSystem.Scheduler

    let initialCancelables = {randomCancel =new Cancelable(myScheduler); autoCancel = new Cancelable(myScheduler) }


    let rec loop (myCancelables:LocalCancelables) = actor {
        let! message = mailbox.Receive ()

        match message with
        | StartRandom ->
            myCancelables.randomCancel.Cancel() //Hit cancel in case there's one already running
            let newCancelables = {myCancelables with randomCancel=new Cancelable(myScheduler)}
       
            consoleWriter <! cnslMsg "Send scheduled messages!" ConsoleColor.Red
            myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.), genAgent, Poke, ActorRefs.Nobody, newCancelables.randomCancel)
            return! loop(newCancelables)

        | StopRandom -> 
            consoleWriter <! cnslMsg "Stop scheduled messages!" ConsoleColor.Red
            myCancelables.randomCancel.Cancel() 
            return! loop(myCancelables)

        | StartAuto ->
            myCancelables.autoCancel.Cancel() //Hit cancel in case there's one already running
            let newCancelables = {myCancelables with autoCancel=new Cancelable(myScheduler)}
       
            consoleWriter <! cnslMsg "Start autopick!" ConsoleColor.Magenta
            myScheduler.ScheduleTellRepeatedly(TimeSpan.Zero, TimeSpan.FromSeconds(1.5), autoPlayer, Poke, ActorRefs.Nobody, newCancelables.autoCancel)
            return! loop(newCancelables)

        | StopAuto ->
            consoleWriter <! cnslMsg "Stop autopick!" ConsoleColor.Magenta
            myCancelables.autoCancel.Cancel()
            return! loop(myCancelables)

        | _ -> return! loop (myCancelables)
        
    }
    loop (initialCancelables)


let randomNumberGenerator (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let randomHandler = select "/user/randomHandler"  mailbox.Context.System

    consoleWriter <! cnslMsg "Random generator created" ConsoleColor.Blue

    let rec loop () = actor {
        let! message = mailbox.Receive()

        match message with
        | Poke -> let newNum = random.Next(1,9)
                  if overDebug then consoleWriter <! cnslMsg (string newNum) ConsoleColor.DarkBlue
                  randomHandler <! Msg.NewRandom newNum
        | _ -> ()
        
        return! loop ()
    }
    loop ()


let communicator (mailbox:Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    consoleWriter <! cnslMsg "Communicator created" ConsoleColor.Blue

    let rec loop () = actor {
        let! msg = mailbox.Receive()

        match msg with
        | GameNums n ->    match n with
                           | RandomNumbers _ -> do (Channel.easySendMessage "numbers" n "Communications Error (Random)") |> ignore
                                                if overDebug then consoleWriter <! cnslMsg "Sent random numbers to Client"  ConsoleColor.DarkBlue

                           | ClickedNumbers _ -> do (Channel.easySendMessage "numbers" n "Communications Error (Clicked)") |> ignore
                                                 if overDebug then consoleWriter <! cnslMsg "Sent clicked numbers to Client"  ConsoleColor.DarkBlue

                           return! loop ()

        //| WriteMessage str -> do (Channel.easySendMessage "message" str) |> ignore

        | GameUpdate g -> do (Channel.easySendMessage "gameUpdate" g "Communications Error (Game Update)") |> ignore
                          if overDebug then consoleWriter <! cnslMsg ("Sent game update to Client - score:" + string g.score)  ConsoleColor.DarkBlue

        | Fail f -> match f with
                    | TooManyNumbers -> do (Channel.easySendMessage "message" "Failed - Too many numbers!") |> ignore
                    | OverTen -> do (Channel.easySendMessage "message" "Failed - Sum was too high!") |> ignore


        | _ -> ()

        return! loop ()

    }
    loop ()


let randomHandler (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let communicator = select "/user/communicator"  mailbox.Context.System
    let validator = select "/user/validator"  mailbox.Context.System

    consoleWriter <! cnslMsg "RandomHandler created" ConsoleColor.Blue

    let rec loop (randomNumbers:GameNumbers) = actor {
        let! msg = mailbox.Receive()


        match msg with
        | NewRandom i -> let newNums = addToGameNumbers randomNumbers i
                         communicator <! GameNums newNums
                         validator <! GameNums newNums //Only needs to happen here as the list grows
                         return! loop (newNums)

        | RemoveNumber i -> let newNums = removeFromGameNumbers randomNumbers i
                            communicator <! GameNums newNums
                            return! loop (newNums)

        | ClearNumbers -> let newNums = RandomNumbers[]
                          communicator <! GameNums newNums
                          if overDebug then consoleWriter <! cnslMsg "Clearing random numbers" ConsoleColor.DarkCyan
                          return! loop(newNums)

        | AskForNumbers -> mailbox.Sender() <! GameNums randomNumbers
                           return! loop(randomNumbers)

        | _ -> return! loop (randomNumbers)

    }
    loop (RandomNumbers[])


let clickedHandler (mailbox: Actor<Msg>) = //genAgent

        let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
        let communicator = select "/user/communicator"  mailbox.Context.System
        let randomHandler = select "/user/randomHandler"  mailbox.Context.System
        let validator = select "/user/validator"  mailbox.Context.System

        consoleWriter <! cnslMsg "ClickedHandler created" ConsoleColor.Blue

        let rec loop (clickedNumbers:GameNumbers) = actor {
            let! msg = mailbox.Receive ()

            match msg with
            | NewClickedNumber n -> let newNums = addToGameNumbers clickedNumbers n.number
                                    if debug then consoleWriter <! cnslMsg (sprintf "You picked %i!" n.number) ConsoleColor.DarkMagenta
                                    randomHandler <! RemoveNumber n
                                    communicator <! GameNums newNums
                                    validator <! GameNums newNums
                                    return! loop (newNums)

            | ClearNumbers -> let newNums = ClickedNumbers[]
                              communicator <! GameNums newNums
                              if overDebug then consoleWriter <! cnslMsg "Clearing clicked numbers" ConsoleColor.DarkCyan
                              return! loop(newNums)

            | _ -> return! loop (clickedNumbers)
                
        }
        loop (ClickedNumbers[])

let validator (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System

    consoleWriter <! cnslMsg "Validator created" ConsoleColor.Blue

    let rec loop() = actor {

        let! message = mailbox.Receive()

        let clickedHandler = select "/user/clickedHandler"  mailbox.Context.System
        let currentGame = select "/user/currentGame" mailbox.Context.System

        match message with
        | GameNums numbers ->     let (a,b) = getLengthAndSum numbers
                                  match numbers with
                                    | ClickedNumbers _ -> match (a,b) with
                                                            | (x, _) when x > 5 -> currentGame <! Fail TooManyNumbers
                                                            | (_,y) when y > 10 -> currentGame <! Fail OverTen
                                                            | (_, 10) -> clickedHandler <! ClearNumbers
                                                                         consoleWriter <! cnslMsg "You made 10!" ConsoleColor.Blue
                                                                         currentGame <! IncrementScore 1
                                                            | _ -> ()
                                          
                                    | RandomNumbers _ -> match (a,b) with
                                                            | (y,_) when y >  10 -> currentGame <! Fail TooManyNumbers
                                                            | _ -> ()

        | _ -> ()
        return! loop()
    }
    loop()


let currentGame (mailbox: Actor<Msg>) =

    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let communicator = select "/user/communicator"  mailbox.Context.System
    let scheduler = select "/user/scheduler"  mailbox.Context.System
    let randomHandler = select "/user/randomHandler"  mailbox.Context.System 
    let clickedHandler = select "/user/clickedHandler"  mailbox.Context.System

    let rec loop(currentGame:Game) = actor {

        let! message = mailbox.Receive()

        match message with
            | IncrementScore x -> let newGame = {currentGame with score = currentGame.score + x}
                                  communicator <! GameUpdate newGame
                                  return! loop(newGame)

            | Fail f -> if debug then consoleWriter <! cnslMsg ("Fail message received!! - " + string f) ConsoleColor.Red 
                        communicator <! f
                        let newGame = {currentGame with cmd = GameCommand.Stop}
                        communicator <! GameUpdate newGame
                        scheduler <! StopRandom
                        scheduler <! StopAuto
                        randomHandler <! ClearNumbers
                        clickedHandler <! ClearNumbers
                        return! loop(newGame)

            | ClearNumbers -> randomHandler <! ClearNumbers
                              clickedHandler <! ClearNumbers
                              if debug then consoleWriter <! cnslMsg "Clearing game numbers" ConsoleColor.Cyan
                              return! loop(currentGame)

            | Start ->  consoleWriter <! cnslMsg "Start Game" ConsoleColor.Green
                        let newGame = {cmd = Continue; score = 0}
                        return! loop(newGame)

            | Restart -> consoleWriter <! cnslMsg "Restart Game" ConsoleColor.Green
                         //randomHandler <! ClearNumbers
                         //clickedHandler <! ClearNumbers
                         let newGame = {cmd = Continue; score = 0}
                         return! loop(newGame)

            | _ -> ()

        return! loop(currentGame)
    }
    loop({cmd = GameCommand.Continue; score = 0})



type autoSum = {indices: int list; sum:int}

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

    let randomHandler = select "/user/randomHandler"  mailbox.Context.System
    let clickedHandler = select "/user/clickedHandler"  mailbox.Context.System
    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System
    let currentGame = select "/user/currentGame" mailbox.Context.System

    consoleWriter <! cnslMsg "Automatic player created" ConsoleColor.Blue

    let rec loop() = actor {

        let! message = mailbox.Receive()
        
        match message with

        | Poke -> randomHandler <! AskForNumbers

        | GameNums numbers -> match numbers with
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
                                                        clickedHandler <! NewClickedNumber myFirstAutoclick
                                                        if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string myFirstAutoclick.number) ConsoleColor.Magenta

                                                        let mySecondAutoclick = {number = n.[myPair.indices.[1]]; listIndex = myPair.indices.[1]-1} //The index is passed on so needs to subtract one, but the number itself isn't
                                                        clickedHandler <! NewClickedNumber mySecondAutoclick
                                                        if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string mySecondAutoclick.number) ConsoleColor.Magenta


                                                        if myPair.indices.Length = 3 then
                                                            let myThirdAutoclick = {number = n.[myPair.indices.[2]]; listIndex = myPair.indices.[2]-2} //The index is passed on so needs to subtract two, but the number itself isn't
                                                            clickedHandler <! NewClickedNumber myThirdAutoclick
                                                            if overDebug then consoleWriter <! cnslMsg ("I picked the number " + string myThirdAutoclick.number) ConsoleColor.Magenta
                                                     else
                                                        consoleWriter <! cnslMsg ("Nothing to pick!") ConsoleColor.DarkMagenta
                                                        if n.Length = 9 then
                                                            consoleWriter <! cnslMsg ("Time to cheat... :-D") ConsoleColor.Red
                                                            currentGame <! ClearNumbers
        | _ -> ()
        
        return! loop()

    }
    loop()




//let testActor inputFunction (mailbox: Actor<Msg>) =

//    let consoleWriter = select "/user/consoleWriter" mailbox.Context.System

//    let rec loop() = actor {

//        let! message = mailbox.Receive()
        
//        match message with
//        | GameNums numbers -> match numbers with
//                                | RandomNumbers n -> consoleWriter <! (cnslMsg <| string (inputFunction n))
//                                | ClickedNumbers n -> consoleWriter <! (cnslMsg <| string (n |> List.length))
//        | _ -> ()
        
//        return! loop()

//    }
//    loop()


//let gamesMaster (mailbox: Actor<ActorMessage>) =
        
//    let rec loop(currentGame:Game) = actor {

//        let! message = mailbox.Receive()

//        match message with
//            | IncrementScore x -> let newGame = {currentGame with score = currentGame.score + x}
//                                  return! loop(newGame)
//            | Fail f -> match f with
//                        | TooManyNumbers -> do (Channel.easySendMessage "message" "Failed - Too many numbers!") |> ignore
//                        | OverTen -> do (Channel.easySendMessage "message" "Failed - Sum was too high!") |> ignore
//                        let newGame = {currentGame with status = NotStarted}
//                        return! loop(newGame)
//            | _ -> ()

//        return! loop(currentGame)
//    }
//    loop(initialGame)

//Take all of the console messages and deal with them concurrently - it keeps the colours in check!
let mailMan (mailbox: Actor<Msg>) =

    let rec loop () = actor {
        let! message = mailbox.Receive ()

        Console.WriteLine "Message Received by Mailman"

        match message with
            | StartRandom -> select "/user/scheduler"  tensSystem <! message
            | StopRandom -> select "/user/scheduler"  tensSystem <! message
            | _ -> ()

        return! loop ()
    }
    loop ()


let spawnActors =
    spawn tensSystem "mailMan" mailMan |> ignore
    spawn tensSystem "consoleWriter" consoleWriter |> ignore
    spawn tensSystem "communicator" communicator |> ignore
    spawn tensSystem "randomHandler" randomHandler  |> ignore
    spawn tensSystem "scheduler" (scheduler) |> ignore
    spawn tensSystem "clickedHandler" clickedHandler |> ignore
    spawn tensSystem "randomGenerator" randomNumberGenerator |> ignore
    spawn tensSystem "validator" validator |> ignore
    spawn tensSystem "currentGame" currentGame |> ignore
    spawn tensSystem "auto" automaticPlayer |> ignore

let stopActors = tensSystem.Stop |> ignore





let forwardMessageToActor next (ctx:HttpContext)= task {
    let! request = ctx.BindModelAsync<Msg>()
    Console.WriteLine "Message being forwarded..."
    select "/user/mailMan"  tensSystem <! request
    let reply = "Clicked number received"
    return! json reply next ctx }



let initialiseActors next (ctx:HttpContext) = task {
    let reply = "Initialise actor system request received"
    //stopActors
    spawnActors
    return! json reply next ctx }

let startCounterActor next (ctx:HttpContext) = task {
    select "/user/scheduler"  tensSystem <! StartRandom
    let reply = "Start random number generation"
    return! json reply next ctx }

let stopCounterActor next (ctx:HttpContext) = task {
    select "/user/scheduler"  tensSystem <! StopRandom
    let reply = "Stop random number generation"
    return! json reply next ctx }

let clearNumbers next (ctx:HttpContext) = task {
    let reply = "Clear numbers request received"
    select "/user/currentGame"  tensSystem <! ClearNumbers
    return! json reply next ctx }

let updateClickedNumbers next (ctx:HttpContext)= task {
    let! request = ctx.BindModelAsync<ClickedNumberIndex>()
    select "/user/clickedHandler"  tensSystem <! NewClickedNumber request
    let reply = "Clicked number received"
    return! json reply next ctx }

let startGame next (ctx:HttpContext) = task {
    let reply = "Start request received"
    select "/user/currentGame"  tensSystem <! Start
    return! json reply next ctx }


let restartGame next (ctx:HttpContext) = task {
    let reply = "Restart request received"
    select "/user/currentGame"  tensSystem <! Restart
    return! json reply next ctx }

let singleAuto next (ctx:HttpContext) = task {
    let reply = "Do an auto pick"
    select "/user/auto"  tensSystem <! Poke
    return! json reply next ctx }


let startAutoPlayer next (ctx:HttpContext) = task {
    select "/user/scheduler"  tensSystem <! StartAuto
    let reply = "Start random number generation"
    return! json reply next ctx }

let stopAutoPlayer next (ctx:HttpContext) = task {
    select "/user/scheduler"  tensSystem <! StopAuto
    let reply = "Stop random number generation"
    return! json reply next ctx }


//let addTestActor inputFunction next (ctx:HttpContext) = task {
//    let reply = "Test actor spawning"
//    spawn tensSystem "testActor" (testActor inputFunction) |> ignore
//    return! json reply next ctx }
    


//type injectedFunction = int list -> int

//let injectedActor (tester:injectedFunction) (mailbox:Actor<int list>) =

//    let rec loop () = actor {
//        let! message = mailbox.Receive ()

//        let output = tester message


//        return! loop ()
//    }
//    loop ()

//let getLength (x:int list) = x.Length

//let myTester = spawn tensSystem "test" <| injectedActor getLength