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

    let buildWsSender (ws:WebSocket) : WsSender =
        fun (message:Msg) ->
            let message = {| Topic = ""; Ref = ""; Payload = message |}
            let message = Thoth.Json.Encode.Auto.toString(0, message)
            ws.send message

    let subscription =
        let sub dispatch =
            let onWebSocketMessage (msg:MessageEvent) =
                let msg = msg.data |> decode<ChannelMessage>
                match msg.Topic with
                | "message" -> msg.Payload |> decode<Msg> |> dispatch
                | _ -> ()

            let rec connect () =
                let url = "ws://localhost:8085/channel"

                //if model.CommunicationMode = Model.BroadcastMode.ViaWebSocket then

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
    let unloadSub (model:Model.Model) =
        let setUnloadEvent dispatch = 
            Browser.Dom.window.onbeforeunload <- (fun _ -> dispatch (SysMsg (CloseEvent model.Player.socketId)))

        Cmd.ofSub setUnloadEvent

module KeyboardEvents =
    let keyPressSub =
        let keyPress dispatch =
            Browser.Dom.window.onkeypress <- (fun a -> dispatch (SysMsg (KeyPress a.key)))
        Cmd.ofSub keyPress


let subs model = Cmd.batch [WindowEvents.unloadSub model
                            KeyboardEvents.keyPressSub
                            Channel.subscription]

module CustomEncoders =

    let inline addDummyCoder<'b> extrasIn =
        let typeName = string typeof<'b>
        let simpleEncoder(_ : 'b) = Encode.string (sprintf "%s function" typeName)
        let simpleDecoder = Decode.fail (sprintf "Decoding is not supported for %s type" typeName)
        extrasIn |> Extra.withCustom simpleEncoder simpleDecoder
        
    let inline buildExtras<'a> extraCoders =
        let myEncoder:Encoder<'a> = Encode.Auto.generateEncoder(extra = extraCoders)
        let myDecoder:Decoder<'a> = Decode.Auto.generateDecoder(extra = extraCoders)
        (myEncoder, myDecoder)

let extras = Extra.empty
                |> CustomEncoders.addDummyCoder<WsSender>
                |> CustomEncoders.buildExtras<Model.Model>


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram Model.init Model.update ViewFeliz.render
|> Program.withSubscription subs 
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebuggerCoders (fst extras) (snd extras)
#endif
|> Program.run
