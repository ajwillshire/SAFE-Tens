namespace Views

open Feliz
open Feliz.Bulma
open Feliz.Bulma.Checkradio
open Fable.FontAwesome

open Shared.DataTypes
open Shared.MessageTypes
open Model
open Views
open ViewComponents


module ViewRunning =

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