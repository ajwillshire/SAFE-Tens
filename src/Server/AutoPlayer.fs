module AutoPlayer

open System
open Microsoft.FSharp.Core.Operators
open Akka.FSharp

open Shared.DataTypes
open Shared.MessageTypes

let debug = true
let overDebug = true

//Overload <! to ensure that only a Msg can be sent using <! (except for the poison pill exception above!)
let private (<!) a (b:Msg) = a<!b

//Add a new operator to make it simpler to pass instructions around the place - Msg | Instruction
let private (<!!) a (b:Instruction) = a <! (Instruction b)

//Helper function to make it easier to send messages to the console
let private cnslMsg m c = WriteToConsole ({Text = m; Colour = int c} |> Complex)

let private random = System.Random()

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
    //let consoleWriter = select "/user/consoleWriter" mailbox.Context
    let mailMan = select "../mailMan"  mailbox.Context
    let currentGame = select "../currentGame" mailbox.Context

    mailMan <! cnslMsg "Automatic player created" ConsoleColor.Blue

    let rec loop() = actor {

        let! message = mailbox.Receive()
        
        match message with

        | Instruction Poke -> randomHandler <!! SendMeNumbers //Asks the Random Number Handler to send the current numbers

        | GameData g -> match g with //Receives the current numbers from the handler

                            | GameNums numbers ->
                                match numbers with
                                | ClickedNumbers _ -> ()
                                | RandomNumbers n -> let myPairs = getSums n
                                                     if myPairs.Length > 0 then
                                                        let myIndex = random.Next(0, myPairs.Length-1)
                                                        let myPair = myPairs.[myIndex]

                                                        if overDebug then 
                                                            if myPair.indices.Length = 3 then
                                                                mailMan <! cnslMsg (sprintf "Indices: %i %i %i" myPair.indices.[0] myPair.indices.[1] myPair.indices.[2]) ConsoleColor.Magenta
                                                            else
                                                                mailMan <! cnslMsg (sprintf "Indices: %i %i" myPair.indices.[0] myPair.indices.[1]) ConsoleColor.Magenta
                                                        
                                                        let myFirstAutoclick = {Number = n.[myPair.indices.[0]]; ListIndex = myPair.indices.[0]}
                                                        clickedHandler <!! NewClickedNumber myFirstAutoclick
                                                        if overDebug then mailMan <! cnslMsg ("I picked the number " + string myFirstAutoclick.Number) ConsoleColor.Magenta

                                                        let mySecondAutoclick = {Number = n.[myPair.indices.[1]]; ListIndex = myPair.indices.[1]-1} //The index is passed on so needs to subtract one, but the number itself isn't
                                                        clickedHandler <!! NewClickedNumber mySecondAutoclick
                                                        if overDebug then mailMan <! cnslMsg ("I picked the number " + string mySecondAutoclick.Number) ConsoleColor.Magenta

                                                        if myPair.indices.Length = 3 then
                                                            let myThirdAutoclick = {Number = n.[myPair.indices.[2]]; ListIndex = myPair.indices.[2]-2} //The index is passed on so needs to subtract two, but the number itself isn't
                                                            clickedHandler <!! NewClickedNumber myThirdAutoclick
                                                            if overDebug then mailMan <! cnslMsg ("I picked the number " + string myThirdAutoclick.Number) ConsoleColor.Magenta

                                                     else
                                                        mailMan <! cnslMsg ("Nothing to pick!") ConsoleColor.DarkMagenta
                                                     if n.Length = 9 then
                                                        mailMan <! cnslMsg ("Time to cheat... :-D") ConsoleColor.Red
                                                        currentGame <!! ClearNumbers
                            | _ -> ()

        | _ -> ()
        
        return! loop()

    }
    loop()

