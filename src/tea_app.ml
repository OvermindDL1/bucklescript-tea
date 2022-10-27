type ('flags, 'model, 'msg) program =
  {
  init: 'flags -> ('model * 'msg Tea_cmd.t) ;
  update: 'model -> 'msg -> ('model * 'msg Tea_cmd.t) ;
  view: 'model -> 'msg Vdom.t ;
  subscriptions: 'model -> 'msg Tea_sub.t ;
  shutdown: 'model -> 'msg Tea_cmd.t }
type ('flags, 'model, 'msg) standardProgram =
  {
  init: 'flags -> ('model * 'msg Tea_cmd.t) ;
  update: 'model -> 'msg -> ('model * 'msg Tea_cmd.t) ;
  view: 'model -> 'msg Vdom.t ;
  subscriptions: 'model -> 'msg Tea_sub.t }
type ('model, 'msg) beginnerProgram =
  {
  model: 'model ;
  update: 'model -> 'msg -> 'model ;
  view: 'model -> 'msg Vdom.t }
type ('model, 'msg) pumpInterface =
  {
  startup: unit -> unit ;
  render_string: 'model -> string ;
  handleMsg: 'model -> 'msg -> 'model ;
  shutdown: 'msg Tea_cmd.t -> unit }
type 'msg programInterface =
  <
    pushMsg: 'msg -> unit  ;shutdown: unit -> unit  ;getHtmlString: unit ->
                                                                    string  
    >  Js.t
external makeProgramInterface :
  pushMsg:('msg -> unit) ->
    shutdown:(unit -> unit) ->
      getHtmlString:(unit -> string) -> 'msg programInterface = ""[@@bs.obj ]
let programStateWrapper initModel pump shutdown =
  let open Vdom in
    let model = ref initModel in
    let callbacks =
      ref
        {
          enqueue = (fun _msg -> Js.log "INVALID enqueue CALL!");
          on = (fun _ -> ())
        } in
    let pumperInterfaceC () = pump callbacks in
    let pumperInterface = pumperInterfaceC () in
    let pending = ((ref None : 'msg list option ref) : 'msg list option ref) in
    let rec handler msg =
      match !pending with
      | None ->
          let () = pending := ((Some ([]))[@explicit_arity ]) in
          let newModel = pumperInterface.handleMsg (!model) msg in
          let () = model := newModel in
          (match !pending with
           | None ->
               failwith
                 "INVALID message queue state, should never be None during message processing!"
           | ((Some ([]))[@explicit_arity ]) -> pending := None
           | ((Some (msgs))[@explicit_arity ]) ->
               let () = pending := None in List.iter handler (List.rev msgs))
      | ((Some (msgs))[@explicit_arity ]) ->
          pending := ((Some ((msg :: msgs)))[@explicit_arity ]) in
    let render_events = ref [] in
    let finalizedCBs =
      (({
          enqueue = (fun msg -> handler msg);
          on =
            (function
             | Render -> List.iter handler (!render_events)
             | ((AddRenderMsg (msg))[@explicit_arity ]) ->
                 render_events := (List.append (!render_events) [msg])
             | ((RemoveRenderMsg (msg))[@explicit_arity ]) ->
                 render_events :=
                   (List.filter (fun mg -> msg != mg) (!render_events)))
        } : 'msg Vdom.applicationCallbacks) : 'msg Vdom.applicationCallbacks) in
    let () = callbacks := finalizedCBs in
    let pi_requestShutdown () =
      let () =
        callbacks :=
          {
            enqueue =
              (fun _msg -> Js.log "INVALID message enqueued when shut down");
            on = (fun _ -> ())
          } in
      let cmd = shutdown (!model) in
      let () = pumperInterface.shutdown cmd in () in
    let render_string () =
      let rendered = pumperInterface.render_string (!model) in rendered in
    let () = pumperInterface.startup () in
    makeProgramInterface ~pushMsg:handler ~shutdown:pi_requestShutdown
      ~getHtmlString:render_string
let programLoop update view subscriptions initModel initCmd =
  function
  | None ->
      (fun callbacks ->
         let oldSub = ref Tea_sub.none in
         let handleSubscriptionChange model =
           let newSub = subscriptions model in
           oldSub := (Tea_sub.run callbacks callbacks (!oldSub) newSub) in
         {
           startup =
             (fun () ->
                let () = Tea_cmd.run callbacks initCmd in
                let () = handleSubscriptionChange initModel in ());
           render_string =
             (fun model ->
                let vdom = view model in
                let rendered = Vdom.renderToHtmlString vdom in rendered);
           handleMsg =
             (fun model ->
                fun msg ->
                  let (newModel, cmd) = update model msg in
                  let () = Tea_cmd.run callbacks cmd in
                  let () = handleSubscriptionChange newModel in newModel);
           shutdown =
             (fun cmd ->
                let () = Tea_cmd.run callbacks cmd in
                let () =
                  oldSub :=
                    (Tea_sub.run callbacks callbacks (!oldSub) Tea_sub.none) in
                ())
         })
  | ((Some (parentNode))[@explicit_arity ]) ->
      (fun callbacks ->
         let priorRenderedVdom = ref [] in
         let latestModel = ref initModel in
         let nextFrameID = ref None in
         let doRender _delta =
           match !nextFrameID with
           | None -> ()
           | ((Some (_id))[@explicit_arity ]) ->
               let newVdom = [view (!latestModel)] in
               let justRenderedVdom =
                 Vdom.patchVNodesIntoElement callbacks parentNode
                   (!priorRenderedVdom) newVdom in
               let () = priorRenderedVdom := justRenderedVdom in
               let () = (!callbacks).on Render in nextFrameID := None in
         let scheduleRender () =
           match !nextFrameID with
           | Some _ -> ()
           | None ->
               let realtimeRendering = false in
               if realtimeRendering
               then
                 let () = nextFrameID := ((Some ((-1)))[@explicit_arity ]) in
                 doRender 16
               else
                 (let id = Web.Window.requestAnimationFrame doRender in
                  let () = nextFrameID := ((Some (id))[@explicit_arity ]) in
                  ()) in
         let clearPnode () =
           while (Js.Array.length (Web.Node.childNodes parentNode)) > 0 do
             match Js.Null.toOption (Web.Node.firstChild parentNode) with
             | None -> ()
             | ((Some (firstChild))[@explicit_arity ]) ->
                 let _removedChild =
                   Web.Node.removeChild parentNode firstChild in
                 ()
             done in
         let oldSub = ref Tea_sub.none in
         let handleSubscriptionChange model =
           let newSub = subscriptions model in
           oldSub := (Tea_sub.run callbacks callbacks (!oldSub) newSub) in
         let handlerStartup () =
           let () = clearPnode () in
           let () = Tea_cmd.run callbacks initCmd in
           let () = handleSubscriptionChange (!latestModel) in
           let () = nextFrameID := ((Some ((-1)))[@explicit_arity ]) in
           let () = doRender 16 in () in
         let render_string model =
           let vdom = view model in
           let rendered = Vdom.renderToHtmlString vdom in rendered in
         let handler model msg =
           let (newModel, cmd) = update model msg in
           let () = latestModel := newModel in
           let () = Tea_cmd.run callbacks cmd in
           let () = scheduleRender () in
           let () = handleSubscriptionChange newModel in newModel in
         let handlerShutdown cmd =
           let () = nextFrameID := None in
           let () = Tea_cmd.run callbacks cmd in
           let () =
             oldSub :=
               (Tea_sub.run callbacks callbacks (!oldSub) Tea_sub.none) in
           let () = priorRenderedVdom := [] in let () = clearPnode () in () in
         {
           startup = handlerStartup;
           render_string;
           handleMsg = handler;
           shutdown = handlerShutdown
         })
let program =
  ((fun { init; update; view; subscriptions; shutdown } ->
      fun pnode ->
        fun flags ->
          let () = Web.polyfills () in
          let (initModel, initCmd) = init flags in
          let opnode = Js.Nullable.toOption pnode in
          let pumpInterface =
            programLoop update view subscriptions initModel initCmd opnode in
          programStateWrapper initModel pumpInterface shutdown : ('flags,
                                                                   'model,
                                                                   'msg)
                                                                   program ->
                                                                   Web.Node.t
                                                                    Js.null_undefined
                                                                    ->
                                                                    'flags ->
                                                                    'msg
                                                                    programInterface) : 
  ('flags, 'model, 'msg) program ->
    Web.Node.t Js.null_undefined -> 'flags -> 'msg programInterface)
let standardProgram =
  ((fun { init; update; view; subscriptions } ->
      fun pnode ->
        fun args ->
          program
            {
              init;
              update;
              view;
              subscriptions;
              shutdown = (fun _model -> Tea_cmd.none)
            } pnode args : ('flags, 'model, 'msg) standardProgram ->
                             Web.Node.t Js.null_undefined ->
                               'flags -> 'msg programInterface) : ('flags,
                                                                    'model,
                                                                    'msg)
                                                                    standardProgram
                                                                    ->
                                                                    Web.Node.t
                                                                    Js.null_undefined
                                                                    ->
                                                                    'flags ->
                                                                    'msg
                                                                    programInterface)
let beginnerProgram =
  ((fun { model; update; view } ->
      fun pnode ->
        fun () ->
          standardProgram
            {
              init = (fun () -> (model, Tea_cmd.none));
              update =
                (fun model -> fun msg -> ((update model msg), Tea_cmd.none));
              view;
              subscriptions = (fun _model -> Tea_sub.none)
            } pnode () : ('model, 'msg) beginnerProgram ->
                           Web.Node.t Js.null_undefined ->
                             unit -> 'msg programInterface) : ('model, 
                                                                'msg)
                                                                beginnerProgram
                                                                ->
                                                                Web.Node.t
                                                                  Js.null_undefined
                                                                  ->
                                                                  unit ->
                                                                    'msg
                                                                    programInterface)
let map func vnode = Vdom.map func vnode