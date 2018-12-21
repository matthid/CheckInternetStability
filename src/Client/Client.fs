module Client

open System

open Elmish
open Elmish.React

open Fable
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Import

open Thoth.Json

open Shared


open Fulma

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model =
  { PingerModel : PingConnection.Model
    SpeedTest : TimeSpan option
    SpeedTestData : (DateTime * int * int) list }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | PingerMessage of PingConnection.Msg
    | SetSpeedTest of TimeSpan option
    | AddSpeedTestDataPoint of DateTime * int * int

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let pingModel, pingCmd = PingConnection.init()
    let initialModel = 
        { PingerModel = pingModel
          SpeedTest = None
          SpeedTestData = [] }
    initialModel, Cmd.batch [ Cmd.map PingerMessage pingCmd ]



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | PingerMessage pingMsg ->
        let updatedPingModel, updatedPingCmd = PingConnection.update true pingMsg currentModel.PingerModel
        let nextModel = { currentModel with PingerModel = updatedPingModel }
        nextModel, Cmd.map PingerMessage updatedPingCmd
    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://mangelmaxime.github.io/Fulma" ] [ str "Fulma" ]
           ]

    p [ ]
        [ strong [] [ str "Internet Connection Test" ]
          str " powered by: "
          components ]
let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let renderLabel (props:obj) =
    str "test"

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Internet Connection Test" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Setup your options") ] ]
                //Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                //    [ Heading.h3 [] [ str ("Speed test interval: " + string model.SpeedTest) ] ]
                PingConnection.viewOptions model.PingerModel (Msg.PingerMessage >> dispatch)
                PingConnection.view true model.PingerModel (Msg.PingerMessage >> dispatch)
              ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

/// Trace all the updates to the console
let withZonePath (program: Program<'arg, 'model, 'msg, 'view>) =
    let zonedInit (arg:'arg) =
        PerfHelpers.runRoot(fun _ ->
            let initModel,cmd = program.init arg
            //Log.toConsole ("Initial state:", initModel)
            initModel,cmd)
        
    let zonedUpdate msg model =
        PerfHelpers.runRoot(fun _ ->
            //Log.toConsole ("New message:", msg)
            let newModel,cmd = program.update msg model
            //Log.toConsole ("Updated state:", newModel)
            newModel,cmd)
        
    let zonedView msg dispatch =
        PerfHelpers.runRoot(fun _ ->
            program.view msg dispatch)

    { program with
        init = zonedInit 
        update = zonedUpdate
        view = zonedView }
PerfHelpers.runRoot(fun _ ->
    Program.mkProgram init update view
#if DEBUG
    //|> Program.withConsoleTrace
    |> Program.withHMR
#endif
    |> Program.withReact "elmish-app"
#if DEBUG
    |> Program.withDebugger
#endif
    |> withZonePath
    |> Program.run
)
