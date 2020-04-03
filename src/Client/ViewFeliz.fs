module ViewFeliz

open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators


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
            Html.span
                [
                
                prop.children
                    [
                    Html.a [prop.href "https://github.com/SAFE-Stack/SAFE-template"; prop.text "SAFE" ]
                    str ", "
                    Html.a [prop.href "https://saturnframework.github.io"; prop.text "Saturn" ]
                    str ", "
                    Html.a [prop.href "http://fable.io"; prop.text "Fable" ]
                    str ", "
                    Html.a [prop.href "https://elmish.github.io"; prop.text "Elmish" ]
                    str ", "
                    Html.a [prop.href "https://getakka.net/"; prop.text "Akka.NET" ]
                    str ", "
                    Html.a [prop.href "https://github.com/Zaid-Ajaj/Feliz"; prop.text "Feliz" ]
                    str ", "
                    Html.a [prop.href "https://github.com/Dzoukr/Feliz.Bulma"; prop.text "Feliz.Bulma" ]
                    str ", "
                    Html.a [prop.href "https://github.com/Fulma/Fulma/tree/master/src/Fable.FontAwesome"; prop.text "Fable.FontAwesome" ]
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
        prop.style [style.margin 20]
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


let renderRunning(model : Running) (dispatchI : Instruction -> unit) =

    //Get these elements ready for later use...
    let buttons = ForEachNumber(model.Numbers) (renderButton dispatchI)

    let clickedButtons = ForEachNumber(model.Clicked) (clickedButtons)

    let scoreBar = "SAFE Tens - Score:" + string model.Points


    //Here's what will be the page layout and what is returned from the function

    Html.div
        [
            Bulma.navbar [
                Bulma.navbarItemDiv [
                        Html.h1
                            [
                                prop.style [style.fontSize 32; style.padding 30]
                                color.isDark
                                prop.text scoreBar
                            ]
                    ]

                ]
            

            Html.div [
                prop.children
                    [
                    Html.h2
                        [
                        prop.style [style.padding 20]    
                        prop.text "Press buttons to start: "
                        ]
                    ]
                prop.style[style.minHeight 50; style.flexShrink 10; style.flexDirection.column]
                ]

            Bulma.columns
                [
                column.is3
                prop.children 
                    [
                    makeButton "Start Random" (fun _ -> dispatchI StartRandom)
                    makeButton "Stop Random" (fun _ -> dispatchI StopRandom)
                    makeButton "Clear" (fun _ -> dispatchI ClearNumbers)
                    ]
                prop.style[style.minHeight 50; style.flexShrink 10; style.flexDirection.row]
                ]
                

            if model.GameType = AdvancedGame then
                Html.div
                    [
                    Html.h2
                        [
                        prop.style [style.padding 20]    
                        prop.text "Advanced Features "
                        ]
                    ]

                Bulma.columns
                    [
                    column.is3
                    prop.children
                        [
                        makeButton "Single Auto" (fun _ -> dispatchI SingleAuto)
                        makeButton "Start Auto" (fun _ -> dispatchI StartAuto)
                        makeButton "Stop Auto" (fun _ -> dispatchI StopAuto)
                        ]
                    ]
            Html.div[
                prop.style [ style.padding 20 ]
                prop.children
                    [
                    Html.div [
                        prop.children[
                            yield! buttons ]
                        prop.style [ style.minHeight 50]
                    ]
                    Html.div [prop.style [style.padding 10]]

                    Html.div [
                        prop.children [
                            yield! clickedButtons ]
                        prop.style [ style.minHeight 50]
                    ]
                ]
            ]

            Bulma.footer [
                prop.children[
                    Bulma.content [safeComponents]
                ]
                prop.style [style.textAlign.center; style.boxShadow (10,10,color.lightSteelBlue)]
            ]
        
        ]


let private renderFinished points (dispatchI : Instruction -> unit) =
    
    Html.div [

        Html.div [ Html.h2 (sprintf "Final Score: %i" points)]

        //With Columns *******************
        
        Html.div[
            prop.style [ style.padding 20 ]
            ]
        Html.div[
            Bulma.columns [
                column.is2
                prop.children [
                    Bulma.button [
                        prop.style [ style.fontSize 20 ; style.backgroundColor color.blueViolet]
                        prop.onClick (fun _ -> dispatchI (StartGame SimpleGame))
                        prop.text "Restart"
                    ]
                    Bulma.button [
                        prop.style [ style.fontSize 20 ; style.backgroundColor color.chartreuse]
                        prop.onClick (fun _ -> dispatchI (StartGame AdvancedGame))
                        prop.text "Restart Advanced"
                    ]
                ]
            ]
        ]
    ]


let private renderNotStarted (state: Model) (dispatchI : Instruction -> unit) =

    Html.div [

        Html.div [ Html.h2 ("Welcome to SAFE Tens")]

        Html.div [
            Bulma.button [
                prop.style [ style.padding 20 ; style.fontSize 20 ]
                prop.onClick (fun _ -> dispatchI (StartGame SimpleGame))
                prop.text "Start"
              ]
            Bulma.button [
                prop.style [ style.padding 20 ; style.fontSize 20 ]
                prop.onClick (fun _ -> dispatchI (StartGame AdvancedGame))
                prop.text "Start Advanced"
            ]
        ]
    ]

//Re-route to the three page layouts based on the Model state
let render (game: Model) (dispatch: Msg -> unit) =

  let dispatchI i = dispatch (Instruction i)

  match game.ModelState with 
  | NotStarted -> renderNotStarted game dispatchI

  | Running state -> renderRunning state dispatchI

  | Finished points -> renderFinished points dispatchI

