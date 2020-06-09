namespace Views

open Shared.MessageTypes
open Model

open Views
open ViewRunning
open ViewNotStarted
open ViewFinished

module ViewRouter =

//Re-route to the three page layouts based on the Model state
    let render (game: Model) (dispatch: Msg -> unit) =

      let dispatchI i = dispatch (Instruction i)
      let dispatchS s = dispatch (SysMsg s)

      match game.ModelState with 
      | NotStarted -> renderNotStarted game dispatchI
      | Running state -> renderRunning state game dispatchI dispatchS
      | FinishedGame gameOver -> renderFinished game gameOver dispatchI

