open System.IO
open Saturn
open Actors
open Giraffe.Core
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open System

open Shared.MessageTypes
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core.Operators
open Giraffe
open Akka.FSharp
open Akka.Actor
open Thoth.Json.Net
open Shared
open TensTypes
open Channel
open Microsoft.Extensions.Configuration


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

//Initialise the actor system
let actorSystem = spawnActors

let forwardMessageToActor next (ctx:HttpContext)= task {
    let! request = ctx.BindModelAsync<PlayerMessage>()
    
    let hub = ctx.GetService<Saturn.Channels.ISocketHub>()
    Console.WriteLine ("Socket used is " + string hub)
    Console.WriteLine ("PlayerID is " + string request.plyr.playerId)

    //All requests go to the gamesMaster
    select  "/user/gamesMaster" actorSystem <! request

    //Send a Msg as a response (this could be an Instruction, Data or just a message)
    let reply = Simple ("Message being forwarded..." + string request.plyr.playerId) |> WriteToConsole

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
            let m = (socketId |> (SetChannelSocketId >> GameData))
            do! (harderSendMessage socketId "message" m "Problem sending SocketId")
            } |> ignore
        return Channels.Ok })


    handle "" (fun ctx message ->
            task {
                let message = message.Payload |> string |> Decode.Auto.unsafeFromString<PlayerMessage>
                let hub = ctx.GetService<Channels.ISocketHub>()
                Console.WriteLine ("(S) Socket used is " + string hub)
                Console.WriteLine ("(S) PlayerID is " + string message.plyr.playerId)

                //All requests go to the gamesMaster
                select  "/user/gamesMaster" actorSystem <! message
            })
     }


let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router tensRouter
    add_channel "/channel" mainChannel
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip

}


run app
