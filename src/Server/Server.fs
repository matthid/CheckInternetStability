open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open Microsoft.AspNetCore.SignalR
open Microsoft.AspNetCore.Hosting


let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let getInitCounter() : Task<Counter> = task { return { Value = 42 } }

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let! counter = getInitCounter()
            return! Successful.OK counter next ctx
        })
}

let configureSerialization (services:IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer())
        |> ignore
    services.AddSignalR() |> ignore
    services

type PingHub() =
    inherit Hub()

    member x.Ping() =
        "pong"
    member x.SendMessage(user:string, message:string) =
        x.Clients.All.SendAsync("messageReceived", user, message)

let app = application {
    service_config configureSerialization
    use_static publicPath
    use_cors "AllowAll" (fun builder ->
        builder.AllowAnyOrigin () |> ignore)
        
    app_config (fun app ->
        app.UseSignalR (fun routes ->
            routes.MapHub<PingHub>(Microsoft.AspNetCore.Http.PathString "/signalr/pinghub")
        )
    )
    url ("http://0.0.0.0:" + port.ToString() + "/")
    memory_cache
    use_gzip
    use_router webApp
}

run app
