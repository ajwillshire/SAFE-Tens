open System.IO
open Saturn
open Actors

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let tensRouter = router {
    post "/api/messages" forwardMessageToActor
    get "/api/random/start" startCounterActor
    get "/api/random/stop" stopCounterActor
    post "/api/clicked" updateClickedNumbers
    get "/api/initialise" initialiseActors
    get "/api/clear" clearNumbers
    get "/api/start" startGame
    get "/api/restart" restartGame
    get "/api/auto" singleAuto
    get "/api/auto/start" startAutoPlayer
    get "/api/auto/stop" stopAutoPlayer
    }


let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router tensRouter
    add_channel "/channel" Router.Channel.mainChannel
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}


run app
