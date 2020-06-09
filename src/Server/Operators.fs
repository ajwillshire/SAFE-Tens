module Operators

open Akka.FSharp
open Akka.Actor

open Shared
open MessageTypes
open DataTypes


//Add an operator to allow sending a PoisonPill
let (<!!!) a (b:PoisonPill) = a <! b

//Overload <! to ensure that only a Msg can be sent using <! (except for the PoisonPill)
let (<!) a (b:Msg) = a<!b

//Add a new operator to make it simpler to pass instructions around the place - Msg | Instruction
let (<!!) a (b:Instruction) = a <! (Instruction b)

//Add a new operator to make it simpler to pass data around the place - Msg | GameData
let (<!&) a (b:GameData) = a <! (GameData b)

//Add a new operator to make it simpler to pass data around the place - Msg | GameData
let (<!%) a (b:PlayerMessage) = a <! (PlayerMessage b)

let cnslMsg m (c:System.ConsoleColor) = WriteToConsole({Text = m; Colour = int c} |> Complex)

let (<<!)  a (m,c) = a <! cnslMsg m c



