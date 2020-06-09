namespace Views

open Fable.React
open Feliz
open Feliz.Bulma
open Shared.DataTypes
open Shared.MessageTypes

module ViewComponents =

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



    let makePlayerTableHeaderRow =
        Html.tableRow[
            prop.children[
                Html.tableCell[prop.text "Player Id"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "Player Name"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "Actor Name"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "High Score"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "Orphaned"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "Adopt?"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
                Html.tableCell[prop.text "Kill?"; prop.style[style.padding 20; style.borderBottom (5, borderStyle.double, color.black)]]
            ]
        ]

    let makePlayerTableRow (dispatchI : Instruction -> unit) (sc:Player)  =
        Html.tableRow[
            prop.style[style.borderBottom (1, borderStyle.solid, color.black)
            ]
            prop.children[
                Html.tableCell[prop.text(string (getPlayerId sc.PlayerId)); prop.style[style.padding 20]]
                Html.tableCell[prop.text(getPlayerName sc.PlayerName); prop.style[style.padding 20]]
                Html.tableCell[prop.text(getActorName sc.ActorName); prop.style[style.padding 20]]
                Html.tableCell[prop.text(getScoreValue sc.HighScore); prop.style[style.padding 20]]
                Html.tableCell[prop.text(string sc.Orphaned); prop.style[style.padding 20]]
                if sc.Orphaned then
                                Html.tableCell[prop.text("Adopt?")
                                               prop.onClick (fun _ -> dispatchI (AdoptPlayer sc.PlayerId))
                                               prop.style[style.padding 20
                                                          style.backgroundColor color.lightGreen
                                                          style.color color.red
                                                          style.textDecoration textDecorationLine.underline
                                                          style.cursor "pointer"
                                               ]
                                ]
                               else
                                Html.tableCell[prop.text("Active");
                                               prop.onClick (fun _ -> dispatchI (AdoptPlayer sc.PlayerId))
                                               prop.style[style.padding 20]
                                ]
                Html.tableCell[prop.text("Kill!")
                               prop.onClick (fun _ -> dispatchI (DeletePlayer sc.PlayerId))
                               prop.style[style.padding 20
                                          style.backgroundColor color.red
                                          style.color color.white
                                          style.textDecoration textDecorationLine.underline
                                          style.cursor "pointer"
                               ]
                ]
            ]
        ]

// ***********************************************

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


// ***********************************************

    let makeButton (txt:string) (onClick) :ReactElement =
        Bulma.button.a [
            button.isFullWidth
            color.isPrimary
            prop.text txt
            prop.onClick onClick
            prop.style [style.margin 30]
        ]