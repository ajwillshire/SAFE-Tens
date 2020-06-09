module GameActors

open Microsoft.FSharp.Core.Operators
open Akka.FSharp
open Akka.Actor

open Shared
open DataTypes
open MessageTypes

type PlayerActor =
    | MailMan
    | RandomGenerator
    | AutoPlayer
    | RandomHandler
    | ClickedHandler
    | Validator
    | Scheduler
    | CurrentGame
    
type SystemActor =
    | ConsoleWriter
    | GamesMaster
    
let getPlayerActorName (actor:PlayerActor) =
    match actor with
        | MailMan -> "mailMan"
        | RandomHandler -> "randomHandler"
        | Scheduler -> "scheduler"
        | ClickedHandler -> "clickedHandler"
        | RandomGenerator -> "randomGenerator"
        | Validator -> "validator"
        | CurrentGame -> "currentGame"
        | AutoPlayer -> "auto"
    
let getSystemActorName (actor:SystemActor) = 
        match actor with
        | ConsoleWriter -> "consoleWriter"
        | GamesMaster -> "gamesMaster"
    
let getSiblingActor (mailbox: Actor<Msg>) (actor:PlayerActor) = select ("../" + getPlayerActorName actor) mailbox.Context
let getChildActor (mailbox: Actor<Msg>)(actor:PlayerActor) = select (getPlayerActorName actor) mailbox.Context
let getParentActor (mailbox: Actor<Msg>) = select ("../") mailbox.Context
let getSystemActor (mailbox: Actor<Msg>) (actor:SystemActor) = select ("/user/" + getSystemActorName actor) mailbox.Context
let getSystemActor2 (system:ActorSystem) (actor:SystemActor) = select ("/user/" + getSystemActorName actor) system

let getActor (mailbox:Actor<_>) (player:Player)  = select ("user/" + (player.refName)) mailbox.Context.System
let getSenderActor (mailbox:Actor<_>) (pm:PlayerMessage) = getActor mailbox pm.sender


let makePlayerActors (mailbox : Actor<Msg>) (player:Player) (assignRoles: PlayerActor -> Player -> (Actor<Msg> -> Cont<Msg, 'a>)) (actors:PlayerActor list) =
    let spawnPlayerActor (mailbox : Actor<Msg>) (player:Player) (role:PlayerActor) =
            spawn mailbox.Context (getPlayerActorName role) (assignRoles role player) //|> ignore
    actors
    |> List.map (spawnPlayerActor mailbox player)
    |> ignore


let makeSystemActors (system:ActorSystem) (assignRoles: SystemActor -> (Actor<Msg> -> Cont<Msg, 'a>)) (actors:SystemActor list) =
    let spawnSystemActor (system : ActorSystem) (role:SystemActor) =
            spawn system (getSystemActorName role) (assignRoles role)
    actors
    |> List.map (spawnSystemActor system)
    |> ignore

