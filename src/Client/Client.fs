module Client

open Browser.Types
open Elmish
open Elmish.React
open Thoth.Json

open Shared
open MessageTypes
open Model
open Shared.TensTypes


module Channel =
    open Browser.WebSocket
    type ChannelMessage = { Topic : string; Payload : string }

    let inline decode<'a> x = x |> unbox<string> |> Decode.Auto.unsafeFromString<'a>

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
                    printfn "WebSocket opened")
                ws.onclose <- (fun ev ->
                    printfn "WebSocket closed. Retrying connection"
                    promise { 
                        do! Promise.sleep 2000
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
            Browser.Dom.window.onkeypress <- (fun a -> dispatch (Instruction (KeyPress a.key)))
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
