module View

open Fable.React
open Fable.React.Props
open Fulma
open Feliz


//Solution References
open Shared
open Model
open CommTypes
open TensTypes
open MessageTypes



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


let renderRunning(model : Running) (dispatchI : Instruction -> unit) =

    //Helper functions for rendering elements
    let renderButton dispatchI index (number : int)   =
        Html.button [
          prop.style [ style.padding 20 ; style.fontSize 20 ]
          prop.onClick (fun _ -> dispatchI (NewClickedNumber {number = number; listIndex = index}))
          prop.text number
        ]

    let clickedButtons index (number : int)   = 
        Html.button [
          prop.style [ style.padding 20 ; style.fontSize 20 ]
          prop.text number
        ] 

    //Get these elements ready for later use...
    let buttons = ForEachNumber(model.Numbers) (renderButton dispatchI)

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
                    Column.column [] [ button "Start Random" (fun _ -> dispatchI StartRandom) ]
                    Column.column [] [ button "Stop Random" (fun _ -> dispatchI StopRandom) ]
                    Column.column [] [ button "Clear" (fun _ -> dispatchI ClearNumbers) ]
                    ]
                Columns.columns []
                    [
                    Column.column [] [ button "Single Auto" (fun _ -> dispatchI SingleAuto) ]
                    Column.column [] [ button "Start Auto" (fun _ -> dispatchI StartAuto) ]
                    Column.column [] [ button "Stop Auto" (fun _ -> dispatchI StopAuto) ]
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



let private renderFinished points (dispatchI : Instruction -> unit) =

    Html.div [
        Html.div [
            Html.h2 [
            prop.text (sprintf "Final Score: %i" points)
            ]
        ]
        Html.button [
            prop.style [ style.padding 20 ; style.fontSize 20 ]
            prop.onClick (fun _ -> dispatchI RestartGame)
            prop.text "Restart"
        ]
            ]


let private renderNotStarted (state: Model) (dispatchI : Instruction -> unit) =

    Html.button [
        prop.style [ style.padding 20 ; style.fontSize 20 ]
        prop.onClick (fun _ -> dispatchI StartGame)
        prop.text "Start"
      ]


//Re-route to the three page layouts based on the Model state
let render (game: Model) (dispatch: Msg -> unit) =

  let dispatchI i = dispatch (Instruction i)

  match game.ModelState with 
  | NotStarted -> renderNotStarted game dispatchI

  | Running state -> renderRunning state dispatchI

  | Finished points -> renderFinished points dispatchI
