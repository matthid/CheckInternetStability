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

type ChatHub() =
    inherit Hub()

    member x.SendMessage(user:string, message:string) =
        x.Clients.All.SendAsync("ReceiveMessage", user, message)

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
    use_cors "AllowAll" (fun builder -> ())

    app_config (fun app ->
        app.UseSignalR (fun routes ->
            routes.MapHub<ChatHub>("/hub")
        )
    )
}

run app
