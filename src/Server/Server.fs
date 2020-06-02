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
open Thoth.Json.Net
open Akka.FSharp

open Shared
open MessageTypes
open TensTypes
open Channel
open ActorManagement
open Saturn.Channels


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

//Initialise the actor system
let actorSystem = spawnActors

//API Routing
let forwardMessageToActor next (ctx:HttpContext)= task {
    let! message = ctx.BindModelAsync<Msg>()
    
    let hub = ctx.GetService<Saturn.Channels.ISocketHub>()
    Console.WriteLine ("Socket used is " + string hub)

    match message with
    | PlayerMessage p -> Console.WriteLine ("(S) PlayerID is " + string p.sender.playerId)
    | _ -> Console.WriteLine ("Non PlayerMessage Received")

    //All requests go to the gamesMaster
    select  "/user/gamesMaster" actorSystem <! message

    //Send a Msg as a response (this could be an Instruction, Data or just a message)
    let reply = match message with
                    | PlayerMessage p -> Simple ("Message being forwarded..." + string p.sender.playerId) |> WriteToConsole
                    | _ -> Simple ("Non PlayerMessage Received") |> WriteToConsole

    return! json reply next ctx }


let tensRouter = router {post "/api/messages" forwardMessageToActor}

//******************************

//Websockets

let mainChannel = channel {
        join (fun ctx clientInfo ->
            task {
            printfn "Connected! Main Socket Id: %O" clientInfo.SocketId
            let hub = ctx.GetService<Channels.ISocketHub>()

            webSocketHub <- Some hub

            task {
                do! Task.Delay 500
                let m = (SocketID clientInfo.SocketId |> (SetChannelSocketId >> SysMsg))
                do! (sendMessageViaHub clientInfo.SocketId m "Problem sending SocketId")
                } |> ignore
            return Channels.Ok })


        handle "" (fun ctx clientInfo message ->
                task {
                    let message = message.Payload |> string |> Decode.Auto.unsafeFromString<Msg>

                    //All requests go to the gamesMaster
                    select  "/user/gamesMaster" actorSystem <! message
                })
        terminate (fun ctx clientInfo ->
                        task {
                            let message = (SocketID clientInfo.SocketId |> (CloseEvent >> SysMsg))
                            select  "/user/gamesMaster" actorSystem <! message

                            Console.WriteLine (sprintf "The user on %s closed the socket." (string clientInfo.SocketId))

                        })
     }

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router tensRouter
    //no_router
    add_channel "/channel" mainChannel
    memory_cache
    use_static publicPath
    use_json_serializer(ThothSerializer())
    use_gzip

}

run app
