module Client

open Browser.Types
open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open System
open Feliz


//Solution References
open Shared
open CommTypes
open TensTypes
open MessageTypes


type Running =
  {
    Clicked : GameNumbers
    Numbers : GameNumbers
    Points : int
  }

type Model =
| NotStarted
| Running of Running 
| Finished of int



module Channel =
    open Browser.WebSocket
    type ChannelMessage = { Topic : string; Payload : string }

    let inline decode<'a> x = x |> unbox<string> |> Thoth.Json.Decode.Auto.unsafeFromString<'a>

    let subscription _ =
        let sub dispatch =
            let onWebSocketMessage (msg:MessageEvent) =
                let msg = msg.data |> decode<ChannelMessage>
                printfn "WebSocket message received: %A" msg
                match msg.Topic with
                | "mainSocketId" -> msg.Payload |> decode<Guid>|> Msg.SetChannelSocketId |> dispatch
                | "numbers" -> msg.Payload |> decode<GameNumbers> |> Msg.GameNums |> dispatch
                //| "clicked"-> msg.Payload |> decode<GameNumbers> |> Msg.GameNums |> dispatch
                | "message" -> msg.Payload |> decode<string> |> ConsoleMessage.Simple |> Msg.WriteToConsole |> dispatch
                | "gameUpdate" -> msg.Payload |> decode<Game> |> Msg.GameUpdate |> dispatch
                | _ -> ()

            let rec connect () =
                let url = "ws://localhost:8085/channel"
                let ws = WebSocket.Create(url)
                ws.onmessage <- onWebSocketMessage
                ws.onopen <- (fun ev -> printfn "WebSocket opened")
                ws.onclose <- (fun ev ->
                    printfn "WebSocket closed. Retrying connection"
                    promise {
                        do! Promise.sleep 2000
                        connect() })

            connect()

        Cmd.ofSub sub


module TensProgram =

    let private withoutCommands model = model, Cmd.none

    let private newGame = Running {Numbers = RandomNumbers []; Clicked = ClickedNumbers []; Points = 0}

    let simpleMsg s = Simple s |> Msg.WriteToConsole

    // defines the initial state and initial command (= side-effect) of the application
    let init () : Model * Cmd<Msg> =
        let initialModel = NotStarted
        //let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/initialise")) () SendToConsole MsgError
        withoutCommands initialModel //, cmd


    // The update function computes the next state of the application based on the current state and the incoming events/messages
    // It can also run side-effects (encoded as commands) like calling the server via Http.
    // these commands in turn, can dispatch messages to which the update function will react.
    let update (msg : Msg) (state : Model) : Model * Cmd<Msg> =
        match state, msg with

        | _, Msg.ClearNumbers ->
             let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/clear")) () Msg.WriteToConsole MsgError
             state, cmd

        | _, Msg.WriteToConsole msg ->
            Console.WriteLine msg
            withoutCommands state

        | _, SetChannelSocketId g ->
            let cmd = Cmd.ofMsg (("Channel socket Id is " + string g) |> simpleMsg)
            state, cmd

        | NotStarted, Msg.Initialise ->
            let cmd = Cmd.batch[Cmd.OfPromise.either (fun () -> Fetch.get ("/api/initialise")) () simpleMsg MsgError
                                Cmd.ofMsg(Msg.Start)] //It's optional whether to also "Start" here, but why not?!
            state, cmd

        | NotStarted, Msg.Start ->
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/start")) () simpleMsg MsgError
            let newState = newGame
            newState, cmd

        | Finished p, Msg.Restart ->
            let commands = Cmd.batch([Cmd.ofMsg(Msg.ClearNumbers)
                                      Cmd.OfPromise.either (fun () -> Fetch.get ("/api/restart")) () simpleMsg MsgError])
            let newState = newGame
            newState, commands

        | Running state, Msg.StartRandom ->
            //let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/random/start")) () simpleMsg MsgError
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", msg)) () simpleMsg MsgError
            Running state, cmd

        | Running state, Msg.StopRandom ->
            //let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/random/stop")) () simpleMsg MsgError
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/messages", msg)) () simpleMsg MsgError
            Running state, cmd

        | Running state, Msg.NewClickedNumber clicked ->
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.post ("/api/clicked", clicked)) () simpleMsg MsgError
            Running state, cmd

        //| Running state, Msg.RandomNums randInts ->
        //     let nextModel = {state with Numbers = randInts}
        //     withoutCommands <| Running nextModel

        //| Running state, Msg.ClickedNums clickedInts ->
        //     let nextModel = {state with Clicked = clickedInts}
        //     withoutCommands <| Running nextModel

        | Running state, Msg.GameNums ints ->

            match ints with
            | RandomNumbers n -> withoutCommands <| Running {state with Numbers = ints}
            | ClickedNumbers n -> withoutCommands <| Running {state with Clicked = ints}

        | Running state, Msg.GameUpdate game ->

            match game.cmd with
            | Continue -> withoutCommands <| Running {state with Points = game.score}
            | GameCommand.Stop -> withoutCommands <| Finished game.score


        | Running state, Msg.SingleAuto ->
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/auto")) () simpleMsg MsgError
            Running state, cmd


        | Running state, Msg.StartAuto ->
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/auto/start")) () simpleMsg MsgError
            Running state, cmd

        | Running state, Msg.StopAuto ->
            let cmd = Cmd.OfPromise.either (fun () -> Fetch.get ("/api/auto/stop")) () simpleMsg MsgError
            Running state, cmd


        | _ -> withoutCommands <| state





module TensView= 

    //Highlighting the tech...
    let safeComponents =
        let components =
            span [ ]
               [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
                   [ str "SAFE  "
                     str Version.template ]
                 str ", "
                 a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
                 str ", "
                 a [ Href "http://fable.io" ] [ str "Fable" ]
                 str ", "
                 a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
                 str ", "
                 a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]

               ]

        span [ ]
            [ str "Version "
              strong [ ] [ str Version.app ]
              str " powered by: "
              components ]

    // ***********************************************

    
    let private button txt onClick =
        Button.button
            [ Button.IsFullWidth
              Button.Color IsPrimary
              Button.OnClick onClick ]
            [ str txt ]


    let renderRunning(model : Running) (dispatch : Msg -> unit) =

        //Helper functions for rendering elements
        let renderButton dispatch index (number : int)   = //dispatch 
            Html.button [
              prop.style [ style.padding 20 ; style.fontSize 20 ]
              prop.onClick (fun _ -> dispatch (Msg.NewClickedNumber {number = number; listIndex = index}))
              prop.text number
            ]

        let clickedButtons index (number : int)   = //dispatch 
            Html.button [
              prop.style [ style.padding 20 ; style.fontSize 20 ]
              prop.text number
            ] 

        //Get these elements ready for later use...
        let buttons = ForEachNumber(model.Numbers) (renderButton dispatch)

        let clickedButtons = ForEachNumber(model.Clicked) (clickedButtons)

        let scoreBar = "SAFE Tens - Score:" + string model.Points

        //Here's what will be the page layout and what is returned from the function
        div []
            [ Navbar.navbar [ Navbar.Color IsGrey ]
                [ Navbar.Item.div [ ]
                    [ Heading.h2 [ ]
                        [ str scoreBar ] ] ]

              Container.container []
                  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                        [ Heading.h3 [] [ str ("Press buttons to start: ") ] ]
                    Columns.columns []
                        [
                        Column.column [] [ button "Random" (fun _ -> dispatch Msg.StartRandom) ]
                        Column.column [] [ button "Stop Random" (fun _ -> dispatch Msg.StopRandom) ]
                        Column.column [] [ button "Clear" (fun _ -> dispatch Msg.ClearNumbers) ]
                        ]
                    Columns.columns []
                        [
                        Column.column [] [ button "Single Auto" (fun _ -> dispatch Msg.SingleAuto) ]
                        Column.column [] [ button "Start Auto" (fun _ -> dispatch Msg.StartAuto) ]
                        Column.column [] [ button "Stop Auto" (fun _ -> dispatch Msg.StopAuto) ]
                        ]
                  ]

              Html.div [
                 yield! buttons
              ]
              Html.div [
                 yield! clickedButtons
              ]

              Footer.footer [ ]
                    [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                        [ safeComponents ] ] ]



    let private renderFinished points dispatch =

        Html.div [
            Html.div [
                Html.h2 [
                prop.text (sprintf "Final Score: %i" points)
                ]
            ]
            Html.button [
                prop.style [ style.padding 20 ; style.fontSize 20 ]
                prop.onClick (fun _ -> dispatch Msg.Restart)
                prop.text "Restart"
            ]
                ]


    let private renderNotStarted (state: Model) (dispatch: Msg -> unit) =

        Html.button [
            prop.style [ style.padding 20 ; style.fontSize 20 ]
            prop.onClick (fun _ -> dispatch Start)
            prop.text "Start"
          ]


    //Re-route to the three page layouts based on the Model state
    let render (state: Model) (dispatch: Msg -> unit) =
      match state with 
      | NotStarted -> renderNotStarted state dispatch

      | Running state -> renderRunning state dispatch

      | Finished points -> renderFinished points dispatch



#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram TensProgram.init TensProgram.update TensView.render
|> Program.withSubscription Channel.subscription

#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
