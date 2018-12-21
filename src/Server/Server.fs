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
open System.Reflection
open System.Threading


let publicPath =
    [ Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName |> fun p -> Path.Combine(p, "public")
      Path.GetFullPath "./public"
      Path.GetFullPath "../Client/public" ]
    |> List.find Directory.Exists

do printfn "public path: %s" publicPath

let port = 8085us

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let counter = 0
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

    // Insipred by https://github.com/SignalR/SignalR/issues/1149#issuecomment-302715234
    let clientResponses = new System.Collections.Concurrent.ConcurrentDictionary<System.Guid, TaskCompletionSource<string>>()
    let clientDisconnects = new System.Collections.Concurrent.ConcurrentDictionary<string, CancellationTokenSource>()



    member x.Ping() =
        "p" // ping from client

    override x.OnConnectedAsync() =
        let b = base.OnConnectedAsync()
        clientDisconnects.AddOrUpdate(x.Context.ConnectionId,
            (fun conId -> new CancellationTokenSource()),
            (fun conId source ->
                // shouldn't happen
                source.Cancel()
                new CancellationTokenSource()))
            |> ignore<CancellationTokenSource>
        task {
            do! b
        }
        :> Task

    override x.OnDisconnectedAsync(exn) =
        match clientDisconnects.TryRemove(x.Context.ConnectionId) with
        | true, tok -> tok.Cancel()
        | _ -> ()
        base.OnDisconnectedAsync(exn)

    

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
