module Router

open Giraffe.Core
open Microsoft.AspNetCore.Http
open Saturn
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open System


module Channel =

    let mutable mainChannelId :Option<Guid> = None
    let mutable mainSocketId: Option<Channels.ISocketHub> = None

    let getSocketHub (ctx:HttpContext) =
        try
            ctx.GetService<Channels.ISocketHub>()
        with _ ->
            { new Channels.ISocketHub with
                member __.SendMessageToClient _ _ _ _ = task { return () }
                member __.SendMessageToClients _ _ _ = task { return () } }

    let sendMessage (hub:Channels.ISocketHub) socketId topic payload = task {
        let message = Thoth.Json.Net.Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClient "/channel" socketId topic message }
      
    let easySendMessage topic payload (onError:string) =
        let socket = mainSocketId
        let channelId = mainChannelId

        match (socket,channelId) with
        | Some s, Some c -> sendMessage s c topic payload |> ignore
        | _,_ -> Console.WriteLine onError


    //Configure main channel
    let mainChannel = channel {
        join (fun ctx socketId -> task {
            printfn "Connected! Main Socket Id: %O" socketId
            let hub = ctx.GetService<Channels.ISocketHub>()
            mainSocketId <- Some hub
            task {
                do! Task.Delay 500
                do! sendMessage hub socketId "mainSocketId" socketId
                mainChannelId <- Some socketId
                }
            |> ignore
            return Channels.Ok }) }