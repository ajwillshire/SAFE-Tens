module ChannelClient

open Browser.Types
open Thoth.Json

open Shared
open MessageTypes

open Browser.WebSocket

type WsSender = Msg -> unit

type ConnectionState =
    | DisconnectedFromServer | ConnectedToServer of WsSender | Connecting
    member this.IsConnected =
        match this with
        | ConnectedToServer _ -> true
        | DisconnectedFromServer | Connecting -> false



let inline decode<'a> x = x |> unbox<string> |> Thoth.Json.Decode.Auto.unsafeFromString<'a>

let buildWsSender (ws:WebSocket) : WsSender =
    fun (message:Msg) ->
        let message = {| Topic = ""; Ref = ""; Payload = message |}
        let message = Encode.Auto.toString(0, message)
        ws.send message

let subscription _ =
    let sub dispatch =
        /// Handles push messages from the server and relays them into Elmish messages.
        let onWebSocketMessage (msg:Msg) =
            //let msg = msg.data |> decode<{| Payload : string |}>
            msg
            |> decode<Msg>
            |> dispatch

        /// Continually tries to connect to the server websocket.
        let rec connect () =
            let ws = WebSocket.Create "ws://localhost:8085/channel"
            ws.onmessage <- onWebSocketMessage
            ws.onopen <- (fun ev ->
                dispatch (ConnectionChange (ConnectedToServer (buildWsSender ws)))
                printfn "WebSocket opened")
            ws.onclose <- (fun ev ->
                dispatch (ConnectionChange DisconnectedFromServer)
                printfn "WebSocket closed. Retrying connection"
                promise {
                    do! Promise.sleep 2000
                    dispatch (ConnectionChange Connecting)
                    connect() })

        connect()

    Cmd.ofSub sub