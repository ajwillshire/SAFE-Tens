namespace Views

open Fable.React
open Feliz
open Feliz.Bulma

open Shared
open Model
open DataTypes
open MessageTypes
open Views
open ViewComponents

module ViewNotStarted =

    let renderNotStarted (state: Model) (dispatchI : Instruction -> unit) =

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

                                match state.Player.PlayerName with
                                | PlayerName None -> ()
                                | _ ->
                                    Html.h2 [
                                        prop.style [ style.padding 20; style.paddingLeft 0 ]
                                    ]
                                    Bulma.button.a [
                                        prop.style[]
                                        prop.onClick (fun _ -> dispatchI NewPlayer)
                                        prop.text "Create player"
                                    ]

                    //If PlayerId is set, the player can start the game
                    | _ ->

                        Html.h2 [
                            prop.text (sprintf "Hello %s!" (getPlayerName state.Player.PlayerName))
                            prop.style [style.padding 40; style.paddingLeft 0]
                        ]

                        Bulma.button.a [
                            color.isPrimary
                            button.isLarge
                            prop.style [style.fontSize 20 ]
                            prop.onClick (fun _ -> dispatchI (StartGame))
                            prop.text "Start"
                        ]

                Html.h2 [
                    prop.style [ style.padding 20; style.paddingLeft 0 ]
                ]

                Bulma.button.a [
                    color.isPrimary
                    button.isLarge
                    prop.style[style.fontSize 20]
                    prop.onClick (fun _ -> dispatchI (SendAllPlayers))
                    prop.text "See all players"
                ]
                Html.h2 [
                    prop.style [ style.padding 30; style.paddingLeft 0 ]
                ]

                match state.GameSystemData.Players with
                | None -> ()
                | Some p ->
                    Html.table[
                            prop.children[
                                thead[] [makePlayerTableHeaderRow]
                                tbody [] [
                                    yield! p.Players |> List.map (makePlayerTableRow dispatchI)
                                ]
                                tfoot[][]
                            ]
                    ]
                    Html.h2 [
                        prop.style [ style.padding 20; style.paddingLeft 0 ]
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