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



    let subscription _ =
        let sub dispatch =
            let onWebSocketMessage (msg:MessageEvent) =
                let msg = msg.data |> decode<ChannelMessage>
                //printfn "WebSocket message received: %A" msg
                match msg.Topic with
                | "message" -> msg.Payload |> decode<Msg> |> dispatch
                | _ -> ()

            let rec connect () =
                let url = "ws://localhost:8085/channel"
                let ws = WebSocket.Create(url)
                ws.onmessage <- onWebSocketMessage
                ws.onopen <- (fun ev ->
                    printfn "WebSocket opened"
                    dispatch (GameData (SetWebSocket ws)))
                ws.onclose <- (fun ev ->
                    printfn "WebSocket closed. Retrying connection"
                    promise { 
                        do! Promise.sleep 2000
                        connect() })

            connect()

        Cmd.ofSub sub



#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Model.init Model.update View.render
|> Program.withSubscription Channel.subscription

#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
