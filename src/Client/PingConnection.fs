module PingConnection

open System

open Elmish
open Elmish.React

open Utils
open Fable
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Import

open Thoth.Json

open Shared


open Fulma


type ConsoleLogger () =
    interface SignalR.ILogger with
        member x.log(logLevel: SignalR.LogLevel, message: string) =
            if logLevel >= SignalR.LogLevel.Warning then
                JS.console.warn(sprintf "SignalR '%A': %s" logLevel message)

let builder = SignalR.signalR.HubConnectionBuilder.Create()
let connection =
    builder
        .configureLogging(SignalR.LogLevel.Warning)
        .configureLogging(new ConsoleLogger())
        .withUrl("/signalr/pinghub")
        .build()

//let signalR = SignalR.signalR

connection.onclose(fun err ->
    JS.console.error("Error SignalR:" + string err))



open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.JS
open Fable.PowerPack.Keyboard

let ping () =
    PerfHelpers.measure (fun () ->
        promise {
            let! p = connection.invoke("Ping", new ResizeArray<_>())
            return p
        }
    )
    |> PromiseModule.map snd
    

connection.on("messageReceived", fun (args:ResizeArray<obj option>) ->
    let username, message = string args.[0].Value, string args.[1].Value
    JS.console.log(sprintf "%s: Message '%s' received" username message)
)

type ChartDataPoint =
  { Delay : float
    TotalDelay : float
    Noise : float
    Time : float
    IsInitial : bool
    IsDisconnect : bool
    IsConnect : bool
    IsError : Exception option }

let createPoint delay time =
  { TotalDelay = delay
    Delay = delay
    Noise = 0.
    Time = time
    IsInitial = false
    IsDisconnect = false
    IsConnect= false
    IsError = None }
    
type DataPoint =
    | Ping of PerfHelpers.MeasureResult
    | PingError of Exception
    | Start
    | ConnectionLost
    | ConnectionEstablished

let epoch = new DateTime(1970, 1, 1, 0, 0, 0)
let fromSimpleDataPoint (ts:DateTime) (dp:DataPoint) =
    let ticks = (ts - epoch).TotalMilliseconds
    match dp with
    | Ping span ->
        { createPoint span.Delay ticks with TotalDelay = span.TotalDelay; Noise = span.Noise }
    | PingError err ->
        { createPoint 0. ticks with IsError = Some err }
    | Start ->
        { createPoint 0. ticks with IsInitial = true }
    | ConnectionLost ->
        { createPoint 0. ticks with IsDisconnect = true }
    | ConnectionEstablished ->
        { createPoint 0. ticks with IsConnect = true }

type Model =
  { PingInterval : TimeSpan
    ChartData : ChartDataPoint list
    CurrentData : ChartDataPoint array
    CurrentMaxEpoch : TimeSpan
    Disconnects : int
    DisconnectedTime : TimeSpan
    PingRunning : bool
    IsConnected : bool
    MaxShowEvents : int
    CSVData : (ChartDataPoint array * DateTime) option }

type Msg =
    | SetPingInterval of TimeSpan
    | AddPingDataPoint of DateTime * Result<PerfHelpers.MeasureResult, Exception>
    | SetConnectionState of Result<unit, Exception>
    | TryConnect
    | IntervalFinished
    | ConnectionClosed of Error
    | SetMaxShowEvents of int
    | GenerateCSV

let pingPromise() =
    Cmd.ofPromise ping ()
        (fun pingTime -> AddPingDataPoint (DateTime.Now, Ok pingTime))
        (fun err ->
            JS.console.error("Error Ping", err)
            AddPingDataPoint (DateTime.Now, Result.Error err))

let registerOnClose () =
    Cmd.ofSub(fun dispatch ->
        connection.onclose (fun error ->
            dispatch (ConnectionClosed error)))
let connectPromise () =
    Cmd.ofPromise connection.start ()
        (fun () -> SetConnectionState (Ok ()))
        (fun err ->
            JS.console.error("Error SignalR", err)
            SetConnectionState (Result.Error err))

let tryReconnectPromise timeout =
    Cmd.ofSub(fun dispatch ->
            JS.setTimeout(fun () -> dispatch TryConnect) timeout
                |> ignore<SetTimeoutToken>
            )

let intervalFinishedPromise timeout =
    Cmd.ofSub (fun dispatch ->
        JS.setTimeout (fun () -> dispatch IntervalFinished) timeout
            |> ignore<SetTimeoutToken>
    )

let init () : Model * Cmd<Msg> =
    let initialModel = 
        { PingInterval = TimeSpan.FromSeconds (0.250)
          PingRunning = false
          MaxShowEvents = 60
          ChartData = [fromSimpleDataPoint DateTime.Now Start]
          CurrentData = [|fromSimpleDataPoint DateTime.Now Start|]
          CurrentMaxEpoch = DateTime.Now - epoch
          Disconnects = 0
          DisconnectedTime = TimeSpan.Zero
          CSVData = None
          IsConnected = false }
    initialModel, Cmd.batch [ registerOnClose(); connectPromise () ]

let getSinceLastDisconnected (model:Model) =
    match model.ChartData |> List.tryFind (fun p -> p.IsDisconnect) with
    | Some p ->
        let ts = TimeSpan.FromMilliseconds p.Time
        DateTime.Now - (epoch + ts)
    | None -> TimeSpan.Zero

let update allowLongRunning (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let addDataPoint date p model =
        //model.ChartData.Add(fromSimpleDataPoint date p)
        //model
        { model with
            ChartData = fromSimpleDataPoint date p :: model.ChartData }

    let newModel, newCmd =
        match msg with
        | SetPingInterval interval ->
            let nextModel = { currentModel with PingInterval = interval }
            nextModel, Cmd.none
        | AddPingDataPoint (date, point) ->
            let dp =
                match point with
                | Ok t -> Ping t
                | Error e -> PingError e
            let nextModel = { currentModel with PingRunning = false } |> addDataPoint date dp
            let timeout = int currentModel.PingInterval.TotalMilliseconds
            nextModel, if currentModel.IsConnected then intervalFinishedPromise timeout else Cmd.none
        | SetConnectionState (Ok ()) ->
            let newDisconnectSpan = getSinceLastDisconnected currentModel
            { currentModel with IsConnected = true; PingRunning = true; DisconnectedTime = currentModel.DisconnectedTime + newDisconnectSpan } |> addDataPoint DateTime.Now ConnectionEstablished, pingPromise()
        | SetConnectionState (Error err) ->
            let timeout = int currentModel.PingInterval.TotalMilliseconds
            currentModel, tryReconnectPromise timeout
        | ConnectionClosed (err) ->
            let timeout = int currentModel.PingInterval.TotalMilliseconds
            { currentModel with IsConnected = false; Disconnects = currentModel.Disconnects + 1 } |> addDataPoint DateTime.Now ConnectionLost, tryReconnectPromise timeout
        | TryConnect ->
            currentModel,
            if currentModel.IsConnected
            then Cmd.none
            else connectPromise ()
        | IntervalFinished ->
            if currentModel.IsConnected
            then { currentModel with PingRunning = true }, pingPromise()
            else currentModel, Cmd.none
        | SetMaxShowEvents ev ->
            let nextModel = { currentModel with MaxShowEvents = ev }
            nextModel, Cmd.none
        | GenerateCSV ->
            let data =
                currentModel.ChartData
                |> List.rev
                |> List.toArray
            { currentModel with CSVData = Some (data, DateTime.Now)}, Cmd.none

    if allowLongRunning && not newModel.PingRunning then
        // re-calculate CurrentData
        let data =
            newModel.ChartData
            |> List.tryTake newModel.MaxShowEvents
            |> List.rev
            |> List.toArray
        
        { newModel with CurrentData = data; CurrentMaxEpoch = TimeSpan.FromMilliseconds((data |> FSharp.Collections.Array.maxBy (fun i -> i.Time)).Time) }, newCmd
    else newModel, newCmd

let viewOptions (model:Model) (dispatch:Dispatch<Msg>) =
    Container.container []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h3 [] [ str ("Ping interval") ]
              Input.number
                [ Input.Option.DefaultValue (string model.PingInterval)
                  Input.Option.OnChange (fun e -> dispatch (Msg.SetPingInterval(TimeSpan.FromMilliseconds <| float e.Value))) ] ]
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h3 [] [ str ("Maximum events to show") ]
              Input.number
                [ Input.Option.DefaultValue (string model.MaxShowEvents)
                  Input.Option.OnChange (fun e -> dispatch (Msg.SetMaxShowEvents <| int e.Value)) ] ] ]

let formatTimespan (ts:TimeSpan) =
    let ticks = ts.Ticks % 1000000000L
    let timeString = sprintf "%02d:%02d:%02d.%7d" ts.Hours ts.Minutes ts.Seconds ticks
    timeString.TrimEnd '0'

let view allowLongRunning (model:Model) (dispatch:Dispatch<Msg>) =
    let data = model.CurrentData
    let nowEpoch = model.CurrentMaxEpoch
    let now = epoch + nowEpoch
    div []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ p [] [ str "Connected: "; str <| string model.IsConnected; br []
                     str "Disconnects: "; str <| string model.Disconnects; br []
                     str "Disconnected Time: "; str (formatTimespan (if model.IsConnected then model.DisconnectedTime else model.DisconnectedTime + getSinceLastDisconnected model)) ]
              ]
        button [ OnClick (fun _ -> dispatch GenerateCSV) ] [ str "Generate CSV" ]
        div [] (match model.CSVData with
                | Some (data, date) ->
                    [ ReactCsv.csvLink [ReactCsv.Data data] [ str "Download data from "; str <| string date ] ]
                | _ -> [])
        PerfHelpers.disableRender (not allowLongRunning || model.PingRunning)
        //div []
          [ Recharts.lineChart
              [ Recharts.Props.Chart.Data data
                Recharts.Props.Chart.Width 600.
                Recharts.Props.Chart.Height 300.
                Recharts.Props.Chart.Margin { top = 20.; bottom = 10.; right = 30.; left = 20. } ]
              [ Recharts.tooltip [] []
                Recharts.legend [] []
                Recharts.xaxis
                  [ Recharts.Props.Cartesian.DataKey "Time"
                    Recharts.Props.Cartesian.Name "Time"
                    Recharts.Props.Cartesian.Type "number"
                    //Recharts.Props.Cartesian.Domain [|"auto" :> obj; nowEpoch.TotalMilliseconds :> obj|]
                    Recharts.Props.Cartesian.Domain [|"dataMin"; "dataMax"|]
                    Recharts.Props.Cartesian.TickFormatter (fun obj ->
                        let tsSinceEpoch = TimeSpan.FromMilliseconds(float obj)
                        let offset = (now - (epoch + tsSinceEpoch)).TotalMilliseconds
                        if offset < 1. then "Now"
                        else
                            let ts = TimeSpan.FromMilliseconds offset
                            sprintf "-%s" (formatTimespan ts))
                    ] []
                
                Recharts.yaxis [] []
                Recharts.line
                  [ Recharts.Props.Cartesian.Type "monotone"
                    Recharts.Props.Cartesian.DataKey "Delay"
                    Recharts.Props.Cartesian.Stroke "#8884d8"
                    //Recharts.Props.Cartesian.Label renderLabel
                    ] []
                Recharts.line
                  [ Recharts.Props.Cartesian.Type "monotone"
                    Recharts.Props.Cartesian.DataKey "Noise"
                    Recharts.Props.Cartesian.Stroke "#ffc658"
                    //Recharts.Props.Cartesian.Label renderLabel
                    ] []
                Recharts.line
                  [ Recharts.Props.Cartesian.Type "monotone"
                    Recharts.Props.Cartesian.DataKey "TotalDelay"
                    Recharts.Props.Cartesian.Stroke "#82ca9d"
                    //Recharts.Props.Cartesian.Label renderLabel
                    ] []
                ]
                ] ]