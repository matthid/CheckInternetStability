module PerfHelpers

open Fable.Core

type Measurement =
    { Zone : ZoneJs.Zone
      Start : float
      mutable Noise : float }

type MeasureResult =
    { TotalDelay : float
      Noise : float
      Delay : float }



open Fable.PowerPack
open Fable.Import

type RootZoneHandle () =
    let mutable measurements : Measurement list = []
    let spec = JsInterop.createEmpty<ZoneJs.ZoneSpec>
    let handleNoise (target:ZoneJs.Zone) f =
        let before: float = Browser.window.performance.now()
        let r = f()
        let after: float = Browser.window.performance.now()
        let noise = after - before
        measurements |> List.iter (fun measure ->
            // ignore current measure and everything started before the measurement
            if measure.Zone <> target && measure.Start < before then
                measure.Noise <- measure.Noise + noise)
        r

    do
        spec.name <- "myroot"
        spec.onInvokeTask <- Some (fun del current target task applyThis applyArgs ->
            let f () = del.invokeTask(target, task, applyThis, applyArgs)
            handleNoise target f)
        spec.onInvoke <- Some(fun del current target func applyThis applyArgs source ->
            let f () = del.invoke(target, func, applyThis, applyArgs, source)
            handleNoise target f)

    let rootZone = ZoneJs.Zone.root.fork(spec)
    member x.RootZoneSpec = spec
    member x.RootZone = rootZone

    member x.Measure startPromise =
        let perfMeasureZone =
            let spec = JsInterop.createEmpty<ZoneJs.ZoneSpec>
            spec.name <- "perfmeasure_" + string (System.Guid.NewGuid())
            spec.onInvokeTask <- Some (fun del current target task applyThis applyArgs ->
                del.invokeTask(target, task, applyThis, applyArgs))
            spec.onInvoke <- Some(fun del current target func applyThis applyArgs source ->
                del.invoke(target, func, applyThis, applyArgs, source))
            rootZone.fork(spec)
        
        promise {
            let! r, d = rootZone.run(fun args ->
                promise {
                    let measurement = { Zone = perfMeasureZone; Start = Browser.window.performance.now(); Noise = 0. }
                    measurements <- measurement :: measurements

                    let! r = startPromise()
                    let after: float = Browser.window.performance.now()
                    measurements <- measurements |> List.filter (fun i -> i.Zone <> perfMeasureZone)
                    let result = after - measurement.Start
                    let measurementResult =
                        { TotalDelay = result
                          Noise = measurement.Noise
                          Delay = result - measurement.Noise }
                    return r, measurementResult
                })
            return r, d
        }

        

let rootZoneHandle = RootZoneHandle()

let measure startPromise = rootZoneHandle.Measure startPromise

let runRoot f = rootZoneHandle.RootZone.run f

open Fable.Import.React
open Fable.Helpers
open Fable.Helpers.React

type DisableProps = { DisableRender : bool }
type DisableState = { ShouldUpdateAfterEnable: bool }

type DisableChildRender(initialProps) as x =
    inherit Component<DisableProps, DisableState>(initialProps)
    do x.setInitState { ShouldUpdateAfterEnable = false }
    override x.shouldComponentUpdate(nextProps, nextState) =
        let somethingChanged = nextProps.DisableRender <> x.props.DisableRender

        if nextProps.DisableRender then
            if not x.state.ShouldUpdateAfterEnable then
                x.setState(fun oldState _ -> { oldState with ShouldUpdateAfterEnable = somethingChanged || oldState.ShouldUpdateAfterEnable })
            false
        else
            
            if not nextState.ShouldUpdateAfterEnable && x.state.ShouldUpdateAfterEnable then
                false
            else somethingChanged || x.state.ShouldUpdateAfterEnable
    override x.componentDidUpdate(prevProps, prevState) =
        if x.state.ShouldUpdateAfterEnable then
            x.setState(fun oldState _ -> { oldState with ShouldUpdateAfterEnable = false })

    override this.render() =
        fragment [] base.children

let inline disableRender doDisable childs = ofType<DisableChildRender,_,_> { DisableRender = doDisable } childs