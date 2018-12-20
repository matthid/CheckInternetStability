// ts2fable 0.6.1
module rec ZoneJs
open System
open Fable.Core
open Fable.Import.JS

let [<Global>] Zone: ZoneType = jsNative

type Function = ResizeArray<obj option> -> unit
type Function<'T> = ResizeArray<obj option> -> 'T

/// Suppress closure compiler errors about unknown 'global' variable
/// Zone is a mechanism for intercepting and keeping track of asynchronous work.
/// 
/// A Zone is a global object which is configured with rules about how to intercept and keep track
/// of the asynchronous callbacks. Zone has these responsibilities:
/// 
/// 1. Intercept asynchronous task scheduling
/// 2. Wrap callbacks for error-handling and zone tracking across async operations.
/// 3. Provide a way to attach data to zones
/// 4. Provide a context specific last frame error handling
/// 5. (Intercept blocking methods)
/// 
/// A zone by itself does not do anything, instead it relies on some other code to route existing
/// platform API through it. (The zone library ships with code which monkey patches all of the
/// browsers's asynchronous API and redirects them through the zone for interception.)
/// 
/// In its simplest form a zone allows one to intercept the scheduling and calling of asynchronous
/// operations, and execute additional code before as well as after the asynchronous task. The rules
/// of interception are configured using [ZoneConfig]. There can be many different zone instances in
/// a system, but only one zone is active at any given time which can be retrieved using
/// [Zone#current].
/// 
/// 
/// 
/// ## Callback Wrapping
/// 
/// An important aspect of the zones is that they should persist across asynchronous operations. To
/// achieve this, when a future work is scheduled through async API, it is necessary to capture, and
/// subsequently restore the current zone. For example if a code is running in zone `b` and it
/// invokes `setTimeout` to scheduleTask work later, the `setTimeout` method needs to 1) capture the
/// current zone and 2) wrap the `wrapCallback` in code which will restore the current zone `b` once
/// the wrapCallback executes. In this way the rules which govern the current code are preserved in
/// all future asynchronous tasks. There could be a different zone `c` which has different rules and
/// is associated with different asynchronous tasks. As these tasks are processed, each asynchronous
/// wrapCallback correctly restores the correct zone, as well as preserves the zone for future
/// asynchronous callbacks.
/// 
/// Example: Suppose a browser page consist of application code as well as third-party
/// advertisement code. (These two code bases are independent, developed by different mutually
/// unaware developers.) The application code may be interested in doing global error handling and
/// so it configures the `app` zone to send all of the errors to the server for analysis, and then
/// executes the application in the `app` zone. The advertising code is interested in the same
/// error processing but it needs to send the errors to a different third-party. So it creates the
/// `ads` zone with a different error handler. Now both advertising as well as application code
/// create many asynchronous operations, but the [Zone] will ensure that all of the asynchronous
/// operations created from the application code will execute in `app` zone with its error
/// handler and all of the advertisement code will execute in the `ads` zone with its error handler.
/// This will not only work for the async operations created directly, but also for all subsequent
/// asynchronous operations.
/// 
/// If you think of chain of asynchronous operations as a thread of execution (bit of a stretch)
/// then [Zone#current] will act as a thread local variable.
/// 
/// 
/// 
/// ## Asynchronous operation scheduling
/// 
/// In addition to wrapping the callbacks to restore the zone, all operations which cause a
/// scheduling of work for later are routed through the current zone which is allowed to intercept
/// them by adding work before or after the wrapCallback as well as using different means of
/// achieving the request. (Useful for unit testing, or tracking of requests). In some instances
/// such as `setTimeout` the wrapping of the wrapCallback and scheduling is done in the same
/// wrapCallback, but there are other examples such as `Promises` where the `then` wrapCallback is
/// wrapped, but the execution of `then` is triggered by `Promise` scheduling `resolve` work.
/// 
/// Fundamentally there are three kinds of tasks which can be scheduled:
/// 
/// 1. [MicroTask] used for doing work right after the current task. This is non-cancelable which is
///     guaranteed to run exactly once and immediately.
/// 2. [MacroTask] used for doing work later. Such as `setTimeout`. This is typically cancelable
///     which is guaranteed to execute at least once after some well understood delay.
/// 3. [EventTask] used for listening on some future event. This may execute zero or more times, with
///     an unknown delay.
/// 
/// Each asynchronous API is modeled and routed through one of these APIs.
/// 
/// 
/// ### [MicroTask]
/// 
/// [MicroTask]s represent work which will be done in current VM turn as soon as possible, before VM
/// yielding.
/// 
/// 
/// ### [TimerTask]
/// 
/// [TimerTask]s represent work which will be done after some delay. (Sometimes the delay is
/// approximate such as on next available animation frame). Typically these methods include:
/// `setTimeout`, `setImmediate`, `setInterval`, `requestAnimationFrame`, and all browser specific
/// variants.
/// 
/// 
/// ### [EventTask]
/// 
/// [EventTask]s represent a request to create a listener on an event. Unlike the other task
/// events they may never be executed, but typically execute more than once. There is no queue of
/// events, rather their callbacks are unpredictable both in order and time.
/// 
/// 
/// ## Global Error Handling
/// 
/// 
/// ## Composability
/// 
/// Zones can be composed together through [Zone.fork()]. A child zone may create its own set of
/// rules. A child zone is expected to either:
/// 
/// 1. Delegate the interception to a parent zone, and optionally add before and after wrapCallback
///     hooks.
/// 2. Process the request itself without delegation.
/// 
/// Composability allows zones to keep their concerns clean. For example a top most zone may choose
/// to handle error handling, while child zones may choose to do user action tracking.
/// 
/// 
/// ## Root Zone
/// 
/// At the start the browser will run in a special root zone, which is configured to behave exactly
/// like the platform, making any existing code which is not zone-aware behave as expected. All
/// zones are children of the root zone.
type [<AllowNullLiteral>] Zone =
    abstract parent: Zone with get, set
    abstract name: string with get, set
    /// <summary>Returns a value associated with the `key`.
    /// 
    /// If the current zone does not have a key, the request is delegated to the parent zone. Use
    /// [ZoneSpec.properties] to configure the set of properties associated with the current zone.</summary>
    /// <param name="key">The key to retrieve.</param>
    abstract get: key: string -> obj option
    /// <summary>Returns a Zone which defines a `key`.
    /// 
    /// Recursively search the parent Zone until a Zone which has a property `key` is found.</summary>
    /// <param name="key">The key to use for identification of the returned zone.</param>
    abstract getZoneWith: key: string -> Zone
    /// <summary>Used to create a child zone.</summary>
    /// <param name="zoneSpec">A set of rules which the child zone should follow.</param>
    abstract fork: zoneSpec: ZoneSpec -> Zone
    /// <summary>Wraps a callback function in a new function which will properly restore the current zone upon
    /// invocation.
    /// 
    /// The wrapped function will properly forward `this` as well as `arguments` to the `callback`.
    /// 
    /// Before the function is wrapped the zone can intercept the `callback` by declaring
    /// [ZoneSpec.onIntercept].</summary>
    /// <param name="callback">the function which will be wrapped in the zone.</param>
    /// <param name="source">A unique debug location of the API being wrapped.</param>
    abstract wrap: callback: 'F * source: string -> 'F
    /// <summary>Invokes a function in a given zone.
    /// 
    /// The invocation of `callback` can be intercepted by declaring [ZoneSpec.onInvoke].</summary>
    /// <param name="callback">The function to invoke.</param>
    /// <param name="applyThis"></param>
    /// <param name="applyArgs"></param>
    /// <param name="source">A unique debug location of the API being invoked.</param>
    abstract run: callback: Function<'T> * ?applyThis: obj option * ?applyArgs: ResizeArray<obj option> * ?source: string -> 'T
    /// <summary>Invokes a function in a given zone and catches any exceptions.
    /// 
    /// Any exceptions thrown will be forwarded to [Zone.HandleError].
    /// 
    /// The invocation of `callback` can be intercepted by declaring [ZoneSpec.onInvoke]. The
    /// handling of exceptions can be intercepted by declaring [ZoneSpec.handleError].</summary>
    /// <param name="callback">The function to invoke.</param>
    /// <param name="applyThis"></param>
    /// <param name="applyArgs"></param>
    /// <param name="source">A unique debug location of the API being invoked.</param>
    abstract runGuarded: callback: Function<'T> * ?applyThis: obj option * ?applyArgs: ResizeArray<obj option> * ?source: string -> 'T
    /// <summary>Execute the Task by restoring the [Zone.currentTask] in the Task's zone.</summary>
    /// <param name="task">to run</param>
    /// <param name="applyThis"></param>
    /// <param name="applyArgs"></param>
    abstract runTask: task: Task * ?applyThis: obj option * ?applyArgs: obj option -> obj option
    /// <summary>Schedule a MicroTask.</summary>
    /// <param name="source"></param>
    /// <param name="callback"></param>
    /// <param name="data"></param>
    /// <param name="customSchedule"></param>
    abstract scheduleMicroTask: source: string * callback: Function * ?data: TaskData * ?customSchedule: (Task -> unit) -> MicroTask
    /// <summary>Schedule a MacroTask.</summary>
    /// <param name="source"></param>
    /// <param name="callback"></param>
    /// <param name="data"></param>
    /// <param name="customSchedule"></param>
    /// <param name="customCancel"></param>
    abstract scheduleMacroTask: source: string * callback: Function * data: TaskData * customSchedule: (Task -> unit) * customCancel: (Task -> unit) -> MacroTask
    /// <summary>Schedule an EventTask.</summary>
    /// <param name="source"></param>
    /// <param name="callback"></param>
    /// <param name="data"></param>
    /// <param name="customSchedule"></param>
    /// <param name="customCancel"></param>
    abstract scheduleEventTask: source: string * callback: Function * data: TaskData * customSchedule: (Task -> unit) * customCancel: (Task -> unit) -> EventTask
    /// <summary>Schedule an existing Task.
    /// 
    /// Useful for rescheduling a task which was already canceled.</summary>
    /// <param name="task"></param>
    abstract scheduleTask: task: 'T -> 'T
    /// <summary>Allows the zone to intercept canceling of scheduled Task.
    /// 
    /// The interception is configured using [ZoneSpec.onCancelTask]. The default canceler invokes
    /// the [Task.cancelFn].</summary>
    /// <param name="task"></param>
    abstract cancelTask: task: Task -> obj option

type [<AllowNullLiteral>] ZoneType =
    abstract current: Zone with get, set
    abstract currentTask: Task with get, set
    /// Verify that Zone has been correctly patched. Specifically that Promise is zone aware.
    abstract assertZonePatched: unit -> unit
    /// Return the root zone.
    abstract root: Zone with get, set

type [<AllowNullLiteral>] UncaughtPromiseError =
    inherit Error
    abstract zone: Zone with get, set
    abstract task: Task with get, set
    abstract promise: Promise<obj option> with get, set
    abstract rejection: obj option with get, set

/// Provides a way to configure the interception of zone events.
/// 
/// Only the `name` property is required (all other are optional).
type [<AllowNullLiteral>] ZoneSpec =
    /// The name of the zone. Useful when debugging Zones.
    abstract name: string with get, set
    /// A set of properties to be associated with Zone. Use [Zone.get] to retrieve them.
    abstract properties: obj option with get, set
    /// Allows the interception of zone forking.
    /// 
    /// When the zone is being forked, the request is forwarded to this method for interception.
    abstract onFork: (ZoneDelegate -> Zone -> Zone -> ZoneSpec -> Zone) option with get, set
    /// Allows interception of the wrapping of the callback.
    abstract onIntercept: (ZoneDelegate -> Zone -> Zone -> Function -> string -> Function) option with get, set
    /// Allows interception of the callback invocation.
    abstract onInvoke: (ZoneDelegate -> Zone -> Zone -> Function -> obj option -> ResizeArray<obj option> -> string -> obj option) option with get, set
    /// Allows interception of the error handling.
    abstract onHandleError: (ZoneDelegate -> Zone -> Zone -> obj option -> bool) option with get, set
    /// Allows interception of task scheduling.
    abstract onScheduleTask: (ZoneDelegate -> Zone -> Zone -> Task -> Task) option with get, set
    abstract onInvokeTask: (ZoneDelegate -> Zone -> Zone -> Task -> obj option -> obj option -> obj option) option with get, set
    /// Allows interception of task cancellation.
    abstract onCancelTask: (ZoneDelegate -> Zone -> Zone -> Task -> obj option) option with get, set
    /// Notifies of changes to the task queue empty status.
    abstract onHasTask: (ZoneDelegate -> Zone -> Zone -> HasTaskState -> unit) option with get, set

/// A delegate when intercepting zone operations.
/// 
/// A ZoneDelegate is needed because a child zone can't simply invoke a method on a parent zone. For
/// example a child zone wrap can't just call parent zone wrap. Doing so would create a callback
/// which is bound to the parent zone. What we are interested in is intercepting the callback before
/// it is bound to any zone. Furthermore, we also need to pass the targetZone (zone which received
/// the original request) to the delegate.
/// 
/// The ZoneDelegate methods mirror those of Zone with an addition of extra targetZone argument in
/// the method signature. (The original Zone which received the request.) Some methods are renamed
/// to prevent confusion, because they have slightly different semantics and arguments.
/// 
/// - `wrap` => `intercept`: The `wrap` method delegates to `intercept`. The `wrap` method returns
///     a callback which will run in a given zone, where as intercept allows wrapping the callback
///     so that additional code can be run before and after, but does not associate the callback
///     with the zone.
/// - `run` => `invoke`: The `run` method delegates to `invoke` to perform the actual execution of
///     the callback. The `run` method switches to new zone; saves and restores the `Zone.current`;
///     and optionally performs error handling. The invoke is not responsible for error handling,
///     or zone management.
/// 
/// Not every method is usually overwritten in the child zone, for this reason the ZoneDelegate
/// stores the closest zone which overwrites this behavior along with the closest ZoneSpec.
/// 
/// NOTE: We have tried to make this API analogous to Event bubbling with target and current
/// properties.
/// 
/// Note: The ZoneDelegate treats ZoneSpec as class. This allows the ZoneSpec to use its `this` to
/// store internal state.
type [<AllowNullLiteral>] ZoneDelegate =
    abstract zone: Zone with get, set
    abstract fork: targetZone: Zone * zoneSpec: ZoneSpec -> Zone
    abstract intercept: targetZone: Zone * callback: Function * source: string -> Function
    abstract invoke: targetZone: Zone * callback: Function * applyThis: obj option * applyArgs: ResizeArray<obj option> * source: string -> obj option
    abstract handleError: targetZone: Zone * error: obj option -> bool
    abstract scheduleTask: targetZone: Zone * task: Task -> Task
    abstract invokeTask: targetZone: Zone * task: Task * applyThis: obj option * applyArgs: obj option -> obj option
    abstract cancelTask: targetZone: Zone * task: Task -> obj option
    abstract hasTask: targetZone: Zone * isEmpty: HasTaskState -> unit

type [<AllowNullLiteral>] HasTaskState =
    abstract microTask: bool with get, set
    abstract macroTask: bool with get, set
    abstract eventTask: bool with get, set
    abstract change: TaskType with get, set

type [<StringEnum>] [<RequireQualifiedAccess>] TaskType =
    | MicroTask
    | MacroTask
    | EventTask

type [<StringEnum>] [<RequireQualifiedAccess>] TaskState =
    | NotScheduled
    | Scheduling
    | Scheduled
    | Running
    | Canceling
    | Unknown

type [<AllowNullLiteral>] TaskData =
    /// A periodic [MacroTask] is such which get automatically rescheduled after it is executed.
    abstract isPeriodic: bool option with get, set
    /// Delay in milliseconds when the Task will run.
    abstract delay: float option with get, set
    /// identifier returned by the native setTimeout.
    abstract handleId: float option with get, set

/// Represents work which is executed with a clean stack.
/// 
/// Tasks are used in Zones to mark work which is performed on clean stack frame. There are three
/// kinds of task. [MicroTask], [MacroTask], and [EventTask].
/// 
/// A JS VM can be modeled as a [MicroTask] queue, [MacroTask] queue, and [EventTask] set.
/// 
/// - [MicroTask] queue represents a set of tasks which are executing right after the current stack
///    frame becomes clean and before a VM yield. All [MicroTask]s execute in order of insertion
///    before VM yield and the next [MacroTask] is executed.
/// - [MacroTask] queue represents a set of tasks which are executed one at a time after each VM
///    yield. The queue is ordered by time, and insertions can happen in any location.
/// - [EventTask] is a set of tasks which can at any time be inserted to the end of the [MacroTask]
///    queue. This happens when the event fires.
type [<AllowNullLiteral>] Task =
    /// Task type: `microTask`, `macroTask`, `eventTask`.
    abstract ``type``: TaskType with get, set
    /// Task state: `notScheduled`, `scheduling`, `scheduled`, `running`, `canceling`, `unknown`.
    abstract state: TaskState with get, set
    /// Debug string representing the API which requested the scheduling of the task.
    abstract source: string with get, set
    /// The Function to be used by the VM upon entering the [Task]. This function will delegate to
    /// [Zone.runTask] and delegate to `callback`.
    abstract invoke: Function with get, set
    /// Function which needs to be executed by the Task after the [Zone.currentTask] has been set to
    /// the current task.
    abstract callback: Function with get, set
    /// Task specific options associated with the current task. This is passed to the `scheduleFn`.
    abstract data: TaskData with get, set
    /// Represents the default work which needs to be done to schedule the Task by the VM.
    /// 
    /// A zone may choose to intercept this function and perform its own scheduling.
    abstract scheduleFn: (Task -> unit) with get, set
    /// Represents the default work which needs to be done to un-schedule the Task from the VM. Not all
    /// Tasks are cancelable, and therefore this method is optional.
    /// 
    /// A zone may chose to intercept this function and perform its own un-scheduling.
    abstract cancelFn: (Task -> unit) with get, set
    abstract zone: Zone
    /// Number of times the task has been executed, or -1 if canceled.
    abstract runCount: float with get, set
    /// Cancel the scheduling request. This method can be called from `ZoneSpec.onScheduleTask` to
    /// cancel the current scheduling interception. Once canceled the task can be discarded or
    /// rescheduled using `Zone.scheduleTask` on a different zone.
    abstract cancelScheduleRequest: unit -> unit

type [<AllowNullLiteral>] MicroTask =
    inherit Task
    abstract ``type``: string with get, set

type [<AllowNullLiteral>] MacroTask =
    inherit Task
    abstract ``type``: string with get, set

type [<AllowNullLiteral>] EventTask =
    inherit Task
    abstract ``type``: string with get, set