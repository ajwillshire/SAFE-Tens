module ViewFeliz

open Fable.React
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Checkradio

//Solution References
open Shared
open Model
open DataTypes
open MessageTypes

//Highlighting the tech...

let safeComponents =

        Html.h3 [
            prop.children
                [
                str "Version "
                Html.strong [str Version.app]
                str " powered by: "
                Html.a [prop.href "https://github.com/SAFE-Stack/SAFE-template"; prop.text "SAFE"]
                str ", "
                Html.a [prop.href "https://saturnframework.github.io"; prop.text "Saturn"]
                str ", "
                Html.a [prop.href "http://fable.io"; prop.text "Fable"]
                str ", "
                Html.a [prop.href "https://elmish.github.io"; prop.text "Elmish"]
                str ", "
                Html.a [prop.href "https://getakka.net/"; prop.text "Akka.NET"]
                str ", "
                Html.a [prop.href "https://github.com/Zaid-Ajaj/Feliz"; prop.text "Feliz"]
                str ", "
                Html.a [prop.href "https://github.com/Dzoukr/Feliz.Bulma"; prop.text "Feliz.Bulma"]
                str ", "
                Html.a [prop.href "https://github.com/Fulma/Fulma/tree/master/src/Fable.FontAwesome"; prop.text "Fable.FontAwesome"]
                ]
            ]


// ***********************************************


let private makeButton (txt:string) (onClick) :ReactElement =
    Bulma.button.a [
        button.isFullWidth
        color.isPrimary
        prop.text txt
        prop.onClick onClick
        prop.style [style.margin 30]
    ]
        

//Helper functions for rendering elements
let renderButton dispatchI index (number : int)   =
    Bulma.button.a [
        prop.style [  style.fontSize 20 ]
        prop.onClick (fun _ -> dispatchI (NewClickedNumber {Number = number; ListIndex = index}))
        prop.text number
    ]

let clickedButtons _ (number : int)   =
    Bulma.button.a [
        color.isSuccess
        prop.style [style.fontSize 20]
        prop.children [
            Html.text number
            Fa.i [Fa.Solid.Star; Fa.PullRight] []
        ]
    ] 


let renderRunning(model : Running) (game:Model) (dispatchI : Instruction -> unit) (dispatchS : SysMsg -> unit) =

    let player = game.Player

    //Get these elements ready for later use...
    let buttons = ForEachNumber(model.Numbers) (renderButton dispatchI)

    let clickedButtons = ForEachNumber(model.Clicked) (clickedButtons)

    let scoreBar = "SAFE Tens - Score:" + (string (getScoreValue model.Points))

    let playerDetails = match getPlayerName player.PlayerName with
                            | "" -> "Unknown Player!"
                            | n -> "Player Name: " + n

    //Here's what will be the page layout and what is returned from the function

    Html.div
        [
        prop.style[style.margin 30]
        prop.children[
            Bulma.navbar [
                Bulma.navbarItem.div [
                        Html.h1 [
                            prop.style [style.fontSize 32; style.padding 30]
                            color.isDark
                            prop.text playerDetails
                        ]
                        Html.h1 [
                            prop.style [style.fontSize 32; style.padding 30]
                            color.isDark
                            prop.text scoreBar
                        ]
                ]
            ]
            

            Html.div [
                prop.style[style.minHeight 50; style.flexShrink 10; style.flexDirection.column]
                prop.children [
                    Html.h2 [
                        prop.style [style.padding 20]    
                        prop.text "Press buttons to start: "
                    ]
                ]
            ]

            Html.div[
                Bulma.field.div [
                    prop.style[style.padding 30; style.outlineColor color.darkSlateBlue; style.outlineWidth 2; style.outlineStyle.double]
                    prop.children[
                        Html.div[
                            Checkradio.radio [
                               prop.style[style.padding 10]
                               prop.id "myradio1"
                               prop.name "radio"
                               prop.onCheckedChange (fun _ -> dispatchS (ChangeView SimpleView))
                               prop.isChecked (game.ViewState = SimpleView)
                               prop.key "SimpleCR"
                            ]
                            Html.label [ prop.htmlFor "myradio1"; prop.text "Simple View"; prop.style[style.padding 15; style.paddingRight 10]]
                        ]
                        Html.div[
                            Checkradio.radio [
                               prop.style[style.padding 10]
                               prop.id "myradio2"
                               prop.name "radio"
                               prop.onCheckedChange (fun _ -> dispatchS (ChangeView AdvancedView))
                               prop.isChecked (game.ViewState = AdvancedView)
                               prop.key "AdvancedCR"
                            ]
                            Html.label [ prop.htmlFor "myradio2"; prop.text "Advanced View"; prop.style[style.padding 15; style.paddingRight 10]]
                        ]
                    ]
                ]

                Html.div[
                    prop.style[style.margin 30]
                    prop.children[
                        Bulma.columns [
                            column.is3
                            prop.style[style.minHeight 50; style.flexShrink 10; style.flexDirection.row]
                            prop.children [
                                makeButton "Start Random" (fun _ -> dispatchI StartRandom)
                                makeButton "Stop Random" (fun _ -> dispatchI StopRandom)
                                makeButton "Clear" (fun _ -> dispatchI ClearNumbers)
                            ]
                        ]

                        if game.ViewState = AdvancedView then
                            Html.h2 [
                                prop.style [style.padding 20]    
                                prop.text "Advanced Features "
                            ]

                            Bulma.columns [
                                column.is3
                                prop.children [
                                    makeButton "Single Auto" (fun _ -> dispatchI SingleAuto)
                                    makeButton "Start Auto" (fun _ -> dispatchI StartAuto)
                                    makeButton "Stop Auto" (fun _ -> dispatchI StopAuto)
                                ]
                            ]
                    ]
                ]

                Html.div [
                    prop.style [ style.padding 20 ]
                    prop.children [
                        Html.div [
                            prop.children[ yield! buttons ]
                            prop.style [ style.minHeight 50]
                        ]
                        Html.div [prop.style [style.padding 10]]
                        Html.div [
                            prop.children [ yield! clickedButtons ]
                            prop.style [ style.minHeight 50]
                        ]
                    ]       
                ]

                Bulma.footer [
                    prop.children[safeComponents]
                    prop.style [style.textAlign.center; style.boxShadow (10,10,color.lightSteelBlue)]
                ]
        
            ]
        ]
]


let makeScoreTableRow (sc:ScoreLog) =
                        Html.tableRow[
                            prop.children[
                                Html.tableCell[prop.text sc.playerName; prop.style[style.padding 20]]
                                Html.tableCell[prop.text sc.actorName; prop.style[style.padding 20]]
                                Html.tableCell[prop.text (getScoreValue sc.highScore); prop.style[style.padding 20]]
                            ]]

let makeScoreTableHeaderRow =
    Html.tableRow[
        prop.children[
            Html.tableCell[prop.text "Player"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Actor"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Score"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
        ]]

let private renderFinished (game:Model) gameOver (dispatchI : Instruction -> unit) =
    
    Html.div [
        prop.style[style.margin 50]
        prop.children[
            Html.h2 [prop.style [ style.padding 40 ]]
            Html.h2 (sprintf "Final Score: %i" (getScoreValue gameOver.FinalScore))
            Html.h2 (sprintf "Reason for failure: %s" (match gameOver.FailReason with
                                                                            | FailMessage.OverTen -> "Those numbers exceeded 10"
                                                                            | FailMessage.TooManyNumbers -> "There were just too many numbers!"
                                                                            | FailMessage.Killed -> match gameOver.Culprit with
                                                                                                        | Some p -> sprintf "%s pulled the plug on your game!" (getPlayerName p.PlayerName)
                                                                                                        | None -> "An unknown person pulled the plug on your game"
                                                                            | Ended -> "You quit!"
                                                                            ))
            Html.h2 [prop.style [ style.padding 40 ]]

            match gameOver.FailReason with
                        //HardStop means that your Actors were deleted - no prospect of Restarting
                        | FailMessage.Killed _ -> makeButton "Re-Register" (fun _ -> dispatchI ReRegister) 
                        | _ ->
                                Bulma.columns [
                                    column.is2
                                    prop.children [
                                        makeButton "Restart " (fun _ ->  [ClearNumbers; StartGame] |> List.map (dispatchI) |> ignore)
                                        makeButton "End Session " (fun _ ->  [KillMeNow; ReRegister] |> List.map (dispatchI) |> ignore)
                                    ]
                                    prop.style[style.margin 0]
                                ]

            Html.h2 [prop.text "System High Scores"; prop.style [ style.padding 30 ]]
            
            Html.table[
                    prop.children[
                        thead[] [makeScoreTableHeaderRow]
                        tbody [] [
                            yield! game.GameSystemData.SystemHighScores |> List.map makeScoreTableRow
                        ]
                        tfoot[][]
                    ]
            ]
        ]
    ]


let makePlayerTableHeaderRow =
    Html.tableRow[
        prop.children[
            Html.tableCell[prop.text "Player Id"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Player Name"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Actor Name"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "High Score"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Orphaned"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            Html.tableCell[prop.text "Adopt?"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
        ]
    ]

let makePlayerTableRow (dispatchI : Instruction -> unit) (sc:Player)  =
    Html.tableRow[
        prop.children[
            Html.tableCell[prop.text(string (getPlayerId sc.PlayerId)); prop.style[style.padding 20]]
            Html.tableCell[prop.text(getPlayerName sc.PlayerName); prop.style[style.padding 20]]
            Html.tableCell[prop.text(getActorName sc.ActorName); prop.style[style.padding 20]]
            Html.tableCell[prop.text(getScoreValue sc.HighScore); prop.style[style.padding 20]]
            Html.tableCell[prop.text(string sc.Orphaned); prop.style[style.padding 20]]
            if sc.Orphaned then
                            Bulma.button.a [
                                    prop.style[]
                                    prop.onClick (fun _ -> dispatchI (AdoptPlayer sc.PlayerId))
                                    prop.text "Adopt Player"
                            ]
                           else
                            Html.tableCell[prop.text("Active"); prop.style[style.padding 20]]
        ]
    ]


let private renderNotStarted (state: Model) (dispatchI : Instruction -> unit) =

    Html.div [
        prop.style [style.margin 50]
        prop.children[

            Html.h2 [
                prop.style[style.fontSize 20]
                prop.text "Welcome to SAFE Tens"
            ]

            Html.h2 [prop.style [ style.padding 20 ]]

            match getPlayerId state.Player.PlayerId with
                //If PlayerId is not set, the player needs to register before proceeding
                | None ->   Html.h2 [
                                prop.style [ style.padding 40; style.paddingLeft 0 ]
                                prop.text "Please enter your name:"
                            ]

                            Bulma.input.text [
                                prop.style[style.padding 30]
                                prop.valueOrDefault (getPlayerName state.Player.PlayerName)
                                prop.onChange(UpdatePlayerName >> dispatchI)
                            ]

                            match getPlayerName state.Player.PlayerName with
                            | "" -> ()
                            | _ ->
                                Html.h2 [
                                    prop.style [ style.padding 40; style.paddingLeft 0 ]
                                ]
                                Bulma.button.a [
                                    prop.style[]
                                    prop.onClick (fun _ -> dispatchI NewPlayer)
                                    prop.text "Create player"
                                ]

                                Bulma.button.a [
                                    prop.style[]
                                    prop.onClick (fun _ -> dispatchI (SendAllPlayers))
                                    prop.text "See other players"
                                ]
                                Html.h2 [
                                    prop.style [ style.padding 40; style.paddingLeft 0 ]
                                ]
                                Html.table[
                                        prop.children[
                                            thead[] [makePlayerTableHeaderRow]
                                            tbody [] [
                                                yield! state.GameSystemData.Players.Players |> List.map (makePlayerTableRow dispatchI)
                                            ]
                                            tfoot[][]
                                        ]
                                ]

                //If PlayerId is set, the player can start the game
                | _ ->

                    Html.h2 [
                        prop.text (sprintf "Hello %s!" (getPlayerName state.Player.PlayerName))
                        prop.style [style.padding 40; style.paddingLeft 0]]

                    Bulma.columns [
                        column.is2
                        prop.children [
                            Bulma.button.a [
                                color.isPrimary
                                button.isLarge
                                prop.style [style.fontSize 20 ]
                                prop.onClick (fun _ -> dispatchI (StartGame))
                                prop.text "Start"
                            ]
                            Bulma.button.a [
                                color.isPrimary
                                button.isLarge
                                prop.style [ style.fontSize 20 ; style.color.red]
                                prop.onClick (fun _ -> dispatchI (DeleteAllOtherPlayers state.Player))
                                prop.text "Kill Everyone Else!!!"
                            ]
                        ]
                    ]
        ]
    ]

//Re-route to the three page layouts based on the Model state
let render (game: Model) (dispatch: Msg -> unit) =

  let dispatchI i = dispatch (Instruction i)
  let dispatchS s = dispatch (SysMsg s)

  match game.ModelState with 
  | NotStarted -> renderNotStarted game dispatchI
  | Running state -> renderRunning state game dispatchI dispatchS
  | FinishedGame gameOver -> renderFinished game gameOver dispatchI

