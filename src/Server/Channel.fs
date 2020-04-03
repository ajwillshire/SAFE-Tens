module Channel

    open Saturn
    open FSharp.Control.Tasks.V2
    open System
    open Thoth.Json.Net
    open Microsoft.Extensions.Logging

    open Shared.MessageTypes

    
    let mutable webSocketHub: Option<Channels.ISocketHub> = None

    let sendMessage (hub:Channels.ISocketHub) socketId topic (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClient "/channel" socketId topic message }

    let broadcastMessage (hub:Channels.ISocketHub) topic (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClients "/channel" topic message }
      
    let sendMessageViaHub (socketId:Guid) topic payload (onError:string) = task{
        let hub = webSocketHub

        match (hub,socketId) with
        | Some s, c -> sendMessage s c topic payload |> ignore
        | _,_ -> Console.WriteLine onError
    }









