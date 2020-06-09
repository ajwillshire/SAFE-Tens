namespace Shared
open System
open System.Text.RegularExpressions

module DataTypes =

    type Score = Score of int
    let getScoreValue(Score i) = i

    type ScoreLog = {playerName:string; actorName:string; highScore: Score}

    type PlayerId = PlayerId of int option
    let setPlayerId i = PlayerId (Some i)
    let getPlayerId (PlayerId i) = i

    type PlayerName = PlayerName of string option
    let setPlayerName s = PlayerName (Some s)
    let getPlayerName (PlayerName n) = match n with
                                        | Some s -> s
                                        | None -> String.Empty

    type ActorName = ActorName of string option
    //let setActorName (name:PlayerName) (id:PlayerId) = ActorName (Some ((getPlayerName name).Replace(" ", "_") + "_" + string (match getPlayerId id with | Some n -> n | None -> -1)))
    let setActorName (name:PlayerName) (id:PlayerId) =  let newName = Regex.Replace((getPlayerName name), @"[^\w\-*+=~_.,;:!@&’()]", "")
                                                        ActorName (Some (newName + "_" + string (match getPlayerId id with | Some n -> n | None -> -1)))
    let getActorName (ActorName n) = match n with
                                        | Some s -> s
                                        | None -> String.Empty

                                        //Regex.Replace(strIn, @"[^\w\-*+=~_.,;:!@&’()]", "", RegexOptions.None, TimeSpan.FromSeconds(1.5))

    type SocketId = SocketId of Guid option
    let setSocketId s = SocketId (Some s)
    let getSocketId (SocketId n) = match n with
                                    | Some g -> g
                                    | None -> Guid.Empty

    type Player =
        {
        PlayerName:PlayerName
        ActorName:ActorName
        PlayerId:PlayerId
        SocketId:SocketId
        Orphaned:Boolean
        HighScore: Score
        }
        member this.refName = getActorName this.ActorName

    let makeNewPlayer (name:string) (id: int) (socketId:Guid) =
        let myName = PlayerName (Some name)
        let myId = PlayerId (Some id)
        {PlayerName = myName; ActorName = (setActorName myName myId); PlayerId = myId; SocketId = setSocketId socketId; Orphaned = false; HighScore = Score 0}

    type PlayerList = 
        {Players:Player list}
        member this.addPlayer p = {Players = this.Players @ [p]}
        member this.updatePlayer modifiedPlayer = {Players = this.Players |> List.map (fun x -> if x.PlayerId = modifiedPlayer.PlayerId then modifiedPlayer else x)}

        member this.removePlayerById id = {Players = this.Players |> List.filter (fun p -> p.PlayerId <> id)}
        member this.removePlayerBySocket socketId = {Players = this.Players |> List.filter (fun p -> p.SocketId <> socketId)}

        member this.getPlayerById id = this.Players |> List.filter (fun p -> p.PlayerId = id) |> List.tryHead
        member this.getPlayerBySocket socketId = this.Players |> List.filter (fun p -> p.SocketId = socketId) |> List.tryHead

        member this.numPlayers = this.Players.Length
        member this.nextId = PlayerId (Some(match this.Players.Length with
                                            | 0 -> 1
                                            | _ -> (this.Players |> List.choose(fun x -> getPlayerId x.PlayerId) |> List.max) + 1
                                            ))

    type GameSystemData =
        {
         PlayerHighScore : Score
         SystemHighScores : ScoreLog list
         Players : PlayerList option
        }

    type GameConfig =
        {
         TargetValue:int
         MaxAllowableClicked:int
        }

    type ViewState =
    | SimpleView
    | AdvancedView

    type ClickedNumberIndex =
        { Number: int
          ListIndex : int}

    type MsgAndColour =  {Text:string; Colour:int}

    type ConsoleMessage =
            | Simple of string
            | Complex of MsgAndColour
            | Error of exn

    type GameNumbers =
        | RandomNumbers of int list
        | ClickedNumbers of int list

    let getNumbers(RandomNumbers(data)|ClickedNumbers(data)) = data

    let getNumberByIndex(RandomNumbers(data)|ClickedNumbers(data)) i = data.[i]

    let ForEachNumber(RandomNumbers(data)|ClickedNumbers(data)) doThis = data |> List.mapi doThis

    let addToGameNumbers(input:GameNumbers) newNum =
        match input with
        | RandomNumbers(data) -> RandomNumbers(data @ [newNum])
        | ClickedNumbers(data) -> ClickedNumbers(data @ [newNum])

    let getLengthAndSum(input:GameNumbers) =
        match input with
        | RandomNumbers(data) | ClickedNumbers(data)-> (data.Length, data |> List.sum)
        
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
        | RandomNumbers(data) -> RandomNumbers(removeAtIndex newNum.ListIndex data)
        | ClickedNumbers(data) -> ClickedNumbers(removeAtIndex newNum.ListIndex data)


