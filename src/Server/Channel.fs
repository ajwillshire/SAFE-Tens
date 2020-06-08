module Channel

    open Saturn
    open FSharp.Control.Tasks.V2
    open System
    open Thoth.Json.Net
    open Microsoft.Extensions.Logging

    open Shared.MessageTypes
    open Shared.DataTypes

    let mutable webSocketHub: Option<Channels.ISocketHub> = None

    let sendMessage (hub:Channels.ISocketHub) socketId (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClient "/channel" socketId "message" message }

    let broadcastMessage (hub:Channels.ISocketHub) (payload:Msg) = task {
        let message = Encode.Auto.toString(0, payload)
        do! hub.SendMessageToClients "/channel" "message" message }

    let sendMessageViaHub (socketId:Guid) payload (onError:string) = task{
        let hub = webSocketHub

        match (hub,socketId) with
        | Some s, c -> sendMessage s c payload |> ignore
        | _,_ -> Console.WriteLine onError
    }

    let sendMessageToPlayerClient (player:Player) msg =
        do sendMessageViaHub (getSocketId player.SocketId) msg (sprintf "Communication error sending %s to %s" (string msg) player.refName) |> ignore









