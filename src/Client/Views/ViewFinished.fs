namespace Views

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
open Views
open ViewComponents


module ViewFinished =

    let renderFinished (game:Model) gameOver (dispatchI : Instruction -> unit) =
        
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
                                //Killed means that your Actors were deleted - no prospect of Restarting with them
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
