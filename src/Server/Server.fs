open System
open System.IO
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core.Operators
open FSharp.Control.Tasks.V2
open Microsoft.AspNetCore.Http

open Giraffe
open Saturn
open Thoth.Json.Giraffe
open Akka.FSharp

open Shared
open MessageTypes
open TensTypes
open Channel
open Actors


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

//Initialise the actor system
let actorSystem = spawnActors

let forwardMessageToActor next (ctx:HttpContext)= task {
    let! message = ctx.BindModelAsync<Msg>()
    
    let hub = ctx.GetService<Saturn.Channels.ISocketHub>()
    Console.WriteLine ("Socket used is " + string hub)

    match message with
    | PlayerMessage p -> Console.WriteLine ("(S) PlayerID is " + string p.plyr.playerId)
    | _ -> Console.WriteLine ("Non PlayerMessage Received")

    //All requests go to the gamesMaster
    select  "/user/gamesMaster" actorSystem <! message

    //Send a Msg as a response (this could be an Instruction, Data or just a message)
    let reply = match message with
                    | PlayerMessage p -> Simple ("Message being forwarded..." + string p.plyr.playerId) |> WriteToConsole
                    | _ -> Simple ("Non PlayerMessage Received") |> WriteToConsole

    return! json reply next ctx }


let tensRouter = router {post "/api/messages" forwardMessageToActor}

let mainChannel = channel {
    join (fun ctx socketId ->
        task {
        printfn "Connected! Main Socket Id: %O" socketId
        let hub = ctx.GetService<Channels.ISocketHub>()

        webSocketHub <- Some hub

        task {
            do! Task.Delay 500
            let m = (SocketID socketId |> (SetChannelSocketId >> GameData))
            do! (sendMessageViaHub socketId "message" m "Problem sending SocketId")
            } |> ignore
        return Channels.Ok })


    //handle "" (fun ctx message ->
    //        task {
    //            let message = message.Payload |> string |> Decode.Auto.unsafeFromString<Msg>
    //            let hub = ctx.GetService<Channels.ISocketHub>()

    //            Console.WriteLine ("(S) Socket used is " + string hub)

    //            match message with
    //            | PlayerMessage p -> Console.WriteLine ("(S) PlayerID is " + string p.plyr.playerId)
    //            | _ -> Console.WriteLine ("Non PlayerMessage Received")

    //            //All requests go to the gamesMaster
    //            select  "/user/gamesMaster" actorSystem <! message
    //        })
     }


let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router tensRouter
    add_channel "/channel" mainChannel
    memory_cache
    use_static publicPath
    use_json_serializer(ThothSerializer())
    use_gzip

}

run app
