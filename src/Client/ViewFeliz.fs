module ViewFeliz

open Fable.React
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.Bulma.Checkradio



//Solution References
open Shared
open Model
open CommTypes
open TensTypes
open MessageTypes

//Highlighting the tech...

let safeComponents =

        Html.span [
            str "Version "
            Html.strong [str Version.app]
            str " powered by: "
            Html.h3
                [
                
                prop.children
                    [
                    Html.a [prop.href "https://github.com/SAFE-Stack/SAFE-template"; prop.text "SAFE"; prop.key "Item1" ]
                    str ", "
                    Html.a [prop.href "https://saturnframework.github.io"; prop.text "Saturn"; prop.key "Item2"  ]
                    str ", "
                    Html.a [prop.href "http://fable.io"; prop.text "Fable"; prop.key "Item3"  ]
                    str ", "
                    Html.a [prop.href "https://elmish.github.io"; prop.text "Elmish"; prop.key "Item4"  ]
                    str ", "
                    Html.a [prop.href "https://getakka.net/"; prop.text "Akka.NET"; prop.key "Item5"  ]
                    str ", "
                    Html.a [prop.href "https://github.com/Zaid-Ajaj/Feliz"; prop.text "Feliz"; prop.key "Item6"  ]
                    str ", "
                    Html.a [prop.href "https://github.com/Dzoukr/Feliz.Bulma"; prop.text "Feliz.Bulma"; prop.key "Item7"  ]
                    str ", "
                    Html.a [prop.href "https://github.com/Fulma/Fulma/tree/master/src/Fable.FontAwesome"; prop.text "Fable.FontAwesome"; prop.key "Item8"  ]
                    ]
                ]
            ]

// ***********************************************


let private makeButton (txt:string) (onClick) :ReactElement =
    Bulma.button [
        button.isFullwidth
        button.isPrimary
        prop.text txt
        prop.onClick onClick
        prop.style [style.margin 30]
    ]
        

//Helper functions for rendering elements
let renderButton dispatchI index (number : int)   =
    Bulma.button [
        prop.style [  style.fontSize 20 ]
        prop.onClick (fun _ -> dispatchI (NewClickedNumber {number = number; listIndex = index}))
        prop.text number
    ]

let clickedButtons _ (number : int)   =
    Bulma.button [
        button.isSuccess
        prop.style [style.fontSize 20]
        prop.children [
            Html.text number
            Fa.i [Fa.Solid.Star; Fa.PullRight] []
        ]
    ] 


let renderRunning(model : Running) (game:Model) (dispatchI : Instruction -> unit) =


    let player = game.Player

    //Get these elements ready for later use...
    let buttons = ForEachNumber(model.Numbers) (renderButton dispatchI)

    let clickedButtons = ForEachNumber(model.Clicked) (clickedButtons)

    let scoreBar = "SAFE Tens - Score:" + string model.Points

    let playerDetails = match getPlayerName player.playerName with
                            | "" -> "Unknown Player!"
                            | n -> "Player Name: " + n



    //Here's what will be the page layout and what is returned from the function

    Html.div
        [
        prop.style[style.margin 30]
        prop.children[
            Bulma.navbar [
                Bulma.navbarItemDiv [
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
                Bulma.field [
                    prop.style[style.padding 30; style.outlineColor color.darkSlateBlue; style.outlineWidth 2; style.outlineStyle.double]
                    prop.children[
                        Html.div[
                            Checkradio.radio [
                               prop.style[style.padding 10]
                               prop.id "myradio1"
                               prop.name "radio"
                               prop.onCheckedChange (fun _ -> dispatchI (ChangeView SimpleView))
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
                               prop.onCheckedChange (fun _ -> dispatchI (ChangeView AdvancedView))
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
                    prop.children[Html.span safeComponents]
                    prop.style [style.textAlign.center; style.boxShadow (10,10,color.lightSteelBlue)]
                ]
        
            ]
        ]
]


let private renderFinished gameOver (dispatchI : Instruction -> unit) =
    
    Html.div [
        prop.style[style.margin 50]
        prop.children[

            Html.h2 [prop.style [ style.padding 40 ]]
            Html.h2 (sprintf "Final Score: %i" (getScoreValue gameOver.finalScore))
            Html.h2 (sprintf "Reason for failure: %s" (match gameOver.failReason with
                                                                            | FailMessage.OverTen -> "Those numbers exceeded 10"
                                                                            | FailMessage.TooManyNumbers -> "There were just too many numbers!"
                                                                            | FailMessage.HardStop -> match gameOver.culprit with
                                                                                                        | Some p -> sprintf "%s pulled the plug on your game!" (getPlayerName p.playerName)
                                                                                                        | None -> "An unknown person pulled the plug on your game"
                                                                            ))
            Html.h2 [prop.style [ style.padding 40 ]]

            match gameOver.failReason with
                        | FailMessage.HardStop _ -> ()
                        | _ ->
                                Bulma.columns [
                                    column.is2
                                    prop.children [
                                        Bulma.button [
                                            button.isPrimary
                                            button.isLarge
                                            prop.style [ style.fontSize 20 ]
                                            prop.onClick (fun _ ->  dispatchI (ClearNumbers)
                                                                    dispatchI (StartGame))
                                            prop.text "Restart"
                                        ]
                                    ]
                                    prop.style[style.margin 0]
                                ]
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


            Html.h2 [
                prop.style [ style.padding 40; style.paddingLeft 0 ]
                prop.text "Please enter your name:"
            ]

            Bulma.textInput [
                prop.style[]
                prop.valueOrDefault (getPlayerName state.Player.playerName)

                prop.onChange(UpdatePlayerName >> dispatchI)

            ]

            Html.h2 [prop.style [ style.padding 20 ]]

            match getPlayerName state.Player.playerName with
            | "" -> ()
            | _ ->
                Bulma.button [
                    prop.style[]
                    prop.onClick (fun _ -> (dispatchI (NewPlayer state.Player)))
                    prop.text "Create player"
                ]

            match getPlayerId state.Player.playerId with
                | None -> () //If playerId is none, it returns -1
                | _ ->

                    Html.h2 [prop.style [ style.padding 40 ]]

                    Bulma.columns [
                        column.is2
                        prop.children [

                            Bulma.button [
                                button.isPrimary
                                button.isLarge
                                prop.style [style.fontSize 20 ]
                                prop.onClick (fun _ -> dispatchI (StartGame))
                                prop.text "Start"
                            ]

                            Bulma.button [
                                button.isPrimary
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

  match game.ModelState with 
  | NotStarted -> renderNotStarted game dispatchI

  | Running state -> renderRunning state game dispatchI

  | Finished gameOver -> renderFinished gameOver dispatchI

