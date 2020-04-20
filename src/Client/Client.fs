module Client

open Browser.Types
open Elmish
open Elmish.React
open Thoth.Json

open Shared
open MessageTypes


module Channel =
    open Browser.WebSocket
    type ChannelMessage = { Topic : string; Payload : string }

    let inline decode<'a> x = x |> unbox<string> |> Decode.Auto.unsafeFromString<'a>

    //type WsSender = Msg -> Unit





    let buildWsSender (ws:WebSocket) : WsSender =
        fun (message:Msg) ->
            let message = {| Topic = ""; Ref = ""; Payload = message |}
            let message = Thoth.Json.Encode.Auto.toString(0, message)
            ws.send message


    let subscription _ =
        let sub dispatch =
            let onWebSocketMessage (msg:MessageEvent) =
                let msg = msg.data |> decode<ChannelMessage>
                match msg.Topic with
                | "message" -> msg.Payload |> decode<Msg> |> dispatch
                | _ -> ()

            let rec connect () =
                let url = "ws://localhost:8085/channel"
                let ws = WebSocket.Create(url)
                ws.onmessage <- onWebSocketMessage
                ws.onopen <- (fun ev ->
                    dispatch (SysMsg (ConnectionChange (ConnectedToServer (buildWsSender ws))))
                    printfn "WebSocket opened")
                ws.onclose <- (fun ev ->
                    dispatch (SysMsg (ConnectionChange DisconnectedFromServer))
                    printfn "WebSocket closed. Retrying connection"
                    promise { 
                        do! Promise.sleep 2000
                        dispatch (SysMsg (ConnectionChange Connecting))
                        connect() })

            connect()

        Cmd.ofSub sub

module WindowEvents =
    let unloadSub _ =
        let setUnloadEvent dispatch = 
            Browser.Dom.window.onbeforeunload <- (fun _ -> dispatch (Instruction KillMeNow))
            
        Cmd.ofSub setUnloadEvent

module KeyboardEvents =
    let keyPressSub _ =
        let keyPress dispatch =
            Browser.Dom.window.onkeypress <- (fun a -> dispatch (SysMsg (KeyPress a.key)))
        Cmd.ofSub keyPress


let subs model = Cmd.batch [WindowEvents.unloadSub model
                            KeyboardEvents.keyPressSub model
                            Channel.subscription model]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram Model.init Model.update ViewFeliz.render
|> Program.withSubscription subs //Channel.subscription


#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
