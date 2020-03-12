namespace Shared

open System


module TensTypes =
    [<CLIMutable>]
    type ClickedNumberIndex =
        { number: int
          listIndex : int}

    //State of the game, held by CurrentGame
    type GameCommand =
        | Continue
        | Stop

    type Game =
        {
         cmd:GameCommand
         score:int
        }

    type msgAndColour =  {msg:string; colour:int}

    type ConsoleMessage =
            | Simple of string
            | Complex of msgAndColour


    

module CommTypes =

    open TensTypes

    //type ClickedNumber = ClickedNumber of int

    type GameNumbers =
        | RandomNumbers of int list
        | ClickedNumbers of int list

    type injectedFunction = int list -> int
  
    let getNumberByIndex(RandomNumbers(data)|ClickedNumbers(data)) i = data.[i]
    let ForEachNumber(RandomNumbers(data)|ClickedNumbers(data)) dothis = data |> List.mapi dothis

    let addToGameNumbers(input:GameNumbers) newNum =
        match input with
        | RandomNumbers(data) -> RandomNumbers(data @ [newNum])
        | ClickedNumbers(data) -> ClickedNumbers(data @ [newNum])

    let getLengthAndSum(input:GameNumbers) =
        match input with
        | RandomNumbers(data) | ClickedNumbers(data)-> (data.Length), (data |> List.sum)
        
    let removeAtIndex index list =
        if index < (list |> List.length) then
          list 
          |> List.mapi (fun i element -> (i <> index, element))
          |> List.filter fst 
          |> List.map snd
        else
          list


    let removeFromGameNumbers(input:GameNumbers) (newNum:ClickedNumberIndex) =
        match input with
        | RandomNumbers(data) -> RandomNumbers(removeAtIndex newNum.listIndex data)
        | ClickedNumbers(data) -> ClickedNumbers(removeAtIndex newNum.listIndex data)



module MessageTypes =

    open CommTypes
    open TensTypes


    //Message from ClickedHandler and RandomHandler to Scorekeeper
    type FailMessage =
        | TooManyNumbers //From RandomHandler
        | OverTen //From ClickedHandler


    //Message from Scorekeeper to Counter for starting/stopping
    type CounterMessage =
        | Start
        | Stop


    type Msg =
    | Initialise

    | Start
    | Restart

    | StartRandom
    | StopRandom

    | NewClickedNumber of ClickedNumberIndex

    //| NewClickedNumbers of ClickedNumberIndex list

    | GameNums of GameNumbers

    | GameUpdate of Game // Only goes from Server -> Client

    | ClearNumbers

    | WriteToConsole of ConsoleMessage

    | MsgError of exn //Internal (Client)
    | SetChannelSocketId of Guid //Internal (Client)

    //| CheckNumbers of GameNumbers //Internal (Server)
    | RemoveNumber of ClickedNumberIndex //Internal (Server)
    | IncrementScore of int //Internal (Server)
    | Fail of FailMessage // Internal (Server)
    | NewRandom of int //Internal (Server) - from random generator to RandomHandler

    | AskForNumbers
    | Poke
    | SingleAuto
    | StartAuto
    | StopAuto








