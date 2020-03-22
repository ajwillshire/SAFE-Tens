module Channel

    open Saturn
    open FSharp.Control.Tasks.V2
    open System
    open Thoth.Json.Net

    open Shared.MessageTypes
    open Microsoft.Extensions.Logging

    let mutable webSocketHub: Option<Channels.ISocketHub> = None

    let sendMessage (hub:Channels.ISocketHub) socketId topic (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClient "/channel" socketId topic message }

    let broadcastMessage (hub:Channels.ISocketHub) topic (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClients "/channel" topic message }
      
    //let easySendMessage topic payload (onError:string) = task{
    //    let socket = webSocketHub
    //    let channelId = webSocketId

    //    match (socket,channelId) with
    //    | Some s, Some c -> sendMessage s c topic payload |> ignore
    //    | _,_ -> Console.WriteLine onError
    //}

    let harderSendMessage (socketId:Guid) topic payload (onError:string) = task{
        let hub = webSocketHub

        match (hub,socketId) with
        | Some s, c -> sendMessage s c topic payload |> ignore
        | _,_ -> Console.WriteLine onError
    }









