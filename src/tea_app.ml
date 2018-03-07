(* "OAK-TEA" Maybe?  For OCaml Application Kernal TEA *)


(* TODO:  Create a new program interface to make the program type just handle init/update/shutdown and such interface
   functionality.  Top level should just be a model change handler, what is currently 'view', and multiple programs
   that can update their own part of the model, of which the entirety can be accessed by view.  Probably should be in a
   Tea.AppEx package or something.  Unsure how to work with compatability with Tea.App, perhaps have Tea.App delegate to
   Tea.AppEx or so as a simple wrapper? *)


module type Program = sig
  type flags
  type model
  type msg
  val init : flags -> model * msg Tea_cmd.t
  val update : model -> msg -> model * msg Tea_cmd.t
  val view : model -> msg Vdom.t
  val subscriptions : model -> msg Tea_sub.t
  val shutdown : model -> msg Tea_cmd.t
end

module Make( M: Program ) = struct

  type pumpInterface = {
    startup : unit -> unit;
    render_string : M.model -> string;
    handleMsg : M.model -> M.msg -> M.model;
    shutdown : M.msg Tea_cmd.t -> unit;
  }

  type programInterface = <
    pushMsg : M.msg -> unit;
  > Js.t

  external makeProgramInterface :
    pushMsg:(M.msg -> unit) ->
    shutdown:(unit -> unit) ->
    getHtmlString:(unit -> string) ->
    programInterface = "" [@@bs.obj]


  let programStateWrapper initModel pump shutdown =
    let open Vdom in
    let model = ref initModel in
    let callbacks = ref { enqueue = fun _msg -> Js.log "INVALID enqueue CALL!" } in
    let pumperInterfaceC () = pump callbacks in
    let pumperInterface = pumperInterfaceC () in
    (* let handler = function
      | None -> ()
      | Some msg ->
        let newModel = pumper !model msg in
        let () = (model := newModel) in
        () in *)
    let pending : M.msg list option ref = ref None in
    let rec handler msg =
      match !pending with
      | None ->
        let () = pending := Some [] in
        (* let () = Js.log ("APP", "mainloop", "pre", !model) in *)
        let newModel = pumperInterface.handleMsg !model msg in
        (* let () = Js.log ("APP", "mainloop", "post", newModel) in *)
        let () = (model := newModel) in
        ( match !pending with
          | None -> failwith "INVALID message queue state, should never be None during message processing!"
          | Some [] -> pending := None
          | Some msgs ->
            let () = pending := None in
            List.iter handler (List.rev msgs)
        )
      | Some msgs -> pending := Some (msg :: msgs) in
    let finalizedCBs : M.msg Vdom.applicationCallbacks = {
      enqueue = fun msg -> handler msg;
    } in
    let () = (callbacks := finalizedCBs) in
    let pi_requestShutdown () =
      let () = callbacks := { enqueue = fun _msg -> Js.log "INVALID message enqueued when shut down" } in
      let cmd = shutdown !model in
      let () = pumperInterface.shutdown cmd in
      () in
    let render_string () =
      let rendered = pumperInterface.render_string !model in
      rendered in
    let () = pumperInterface.startup () in
    makeProgramInterface
      ~pushMsg:handler
      ~shutdown:pi_requestShutdown
      ~getHtmlString:render_string


  let programLoop update view subscriptions initModel initCmd = function
    | None -> fun callbacks ->
      let oldSub = ref Tea_sub.none in
      let handleSubscriptionChange model =
        (* let open Vdom in *)
        let newSub = subscriptions model in
        oldSub := (Tea_sub.run callbacks callbacks !oldSub newSub) in
      { startup =
          ( fun () ->
              let () = Tea_cmd.run callbacks initCmd in
              let () = handleSubscriptionChange initModel in
              ()
          )
      ; render_string =
          ( fun model ->
              let vdom = view model in
              let rendered = Vdom.renderToHtmlString vdom in
              rendered
          )
      ; handleMsg =
          ( fun model msg ->
              let newModel, cmd = update model msg in
              (* let open Vdom in *)
              let () = Tea_cmd.run callbacks cmd in
              let () = handleSubscriptionChange newModel in
              newModel
          )
      ; shutdown = (fun cmd ->
            let () = Tea_cmd.run callbacks cmd in (* TODO:  Perhaps add cancelable commands? *)
            let () = oldSub := (Tea_sub.run callbacks callbacks !oldSub Tea_sub.none) in
            ()
          )
      }
    | Some parentNode -> fun callbacks ->
      (* let priorRenderedVdom = ref [view initModel] in *)
      let priorRenderedVdom = ref [] in
      (* let lastVdom = ref (!priorRenderedVdom) in *)
      let latestModel = ref initModel in
      let nextFrameID = ref None in
      let doRender _delta =
        match !nextFrameID with
        | None -> () (* The render has been canceled, possibly by shutting down, do nothing *)
        | Some _id ->
          let newVdom = [view !latestModel] in
          let justRenderedVdom = Vdom.patchVNodesIntoElement callbacks parentNode !priorRenderedVdom newVdom in
          let () = priorRenderedVdom := justRenderedVdom in
          (* let () = Vdom.patchVNodesIntoElement callbacks parentNode !priorRenderedVdom !lastVdom in
          let () = priorRenderedVdom := (!lastVdom) in *)
          (nextFrameID := None) in
      let scheduleRender () = match !nextFrameID with
        | Some _ -> () (* A frame is already scheduled, nothing to do *)
        | None ->
          if false then (* This turns on or off requestAnimationFrame or real-time rendering, false for the benchmark, should be true about everywhere else. *)
            let id = Web.Window.requestAnimationFrame doRender in
            let () = nextFrameID := Some id in
            ()
          else
            let () = nextFrameID := Some (-1) in
            doRender 16 in
      (* let () = Js.log (Vdom.createVNodeIntoElement callbacks !lastVdom parentNode) in *)
      (* We own the passed in node, clear it out TODO:  Clear it out properly *)
      (* let () = Js.log ("Blah", Web.Node.firstChild parentNode, Js.Null.test (Web.Node.firstChild parentNode), false, true) in *)
      let clearPnode () = while (Js.Array.length (Web.Node.childNodes parentNode)) > 0 do
          match Js.Null.to_opt (Web.Node.firstChild parentNode) with
          | None -> ()
          | Some firstChild -> let _removedChild = Web.Node.removeChild parentNode firstChild in ()
        done in
      (* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!lastVdom) in *)
      (* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!priorRenderedVdom) in *)
      (*  Initial render *)
      let oldSub = ref Tea_sub.none in
      let handleSubscriptionChange model =
        (* let open Vdom in *)
        let newSub = subscriptions model in
        oldSub := (Tea_sub.run callbacks callbacks !oldSub newSub) in
      let handlerStartup () =
        let () = clearPnode () in
        let () = Tea_cmd.run callbacks initCmd in
        let () = handleSubscriptionChange !latestModel in
        let () = nextFrameID := Some (-1) in
        let () = doRender 16 in
        () in
      let render_string model =
        let vdom = view model in
        let rendered = Vdom.renderToHtmlString vdom in
        rendered in
      let handler model msg =
        let newModel, cmd = update model msg in
        let () = latestModel := newModel in
        (* let open Vdom in *)
        (* let () = Js.log ("APP", "latestModel", "precmd", !latestModel) in *)
        let () = Tea_cmd.run callbacks cmd in
        (* let () = Js.log ("APP", "latestModel", "postcmd", !latestModel) in *)
        (* TODO:  Figure out if it is better to get view on update like here, or do it in doRender... *)
        (* let newVdom = view newModel in (* Process VDom diffs here with callbacks *) *)
        (* let () = Vdom.patchVNodeIntoElement callbacks parentNode !lastVdom newVdom in *)
        (* let () = Js.log lastVdom in *)
        (* let () = Js.log newVdom in *)
        (* let () = Js.log (Vdom.createVNodeIntoElement callbacks newVdom parentNode) in *)
        (* let () = lastVdom := [newVdom] in *)
        let () = scheduleRender () in
        (* let () = Js.log ("APP", "latestModel", "presub", !latestModel) in *)
        let () = handleSubscriptionChange newModel in
        (* let () = Js.log ("APP", "latestModel", "postsub", !latestModel) in *)
        newModel in
      let handlerShutdown cmd =
        (* let open Vdom in *)
        let () = nextFrameID := None in
        let () = Tea_cmd.run callbacks cmd in
        let () = oldSub := (Tea_sub.run callbacks callbacks !oldSub Tea_sub.none) in
        let () = priorRenderedVdom := [] in
        let () = clearPnode () in
        () in
      { startup = handlerStartup
      ; render_string = render_string
      ; handleMsg = handler
      ; shutdown = handlerShutdown
      }


  let program : Web.Node.t Js.null_undefined -> M.flags -> programInterface =
    fun pnode flags ->
    let () = Web.polyfills () in
    let initModel, initCmd = M.init flags in
    let opnode = Js.Null_undefined.to_opt pnode in
    let pumpInterface = programLoop M.update M.view M.subscriptions initModel initCmd opnode in
    programStateWrapper initModel pumpInterface M.shutdown

  let map func vnode =
    Vdom.map func vnode

end

module type StandardProgram = sig
  type flags
  type model
  type msg
  val init : flags -> model * msg Tea_cmd.t
  val update : model -> msg -> model * msg Tea_cmd.t
  val view : model -> msg Vdom.t
  val subscriptions : model -> msg Tea_sub.t
end

module type BeginnerProgram = sig
  type model
  type msg
  val init : model
  val update : model -> msg -> model
  val view : model -> msg Vdom.t
end

module OfStandard( M: StandardProgram ) : Program = struct
    type flags = M.flags
    type msg = M.msg
    type model = M.model
    let init = M.init
    let view = M.view
    let update = M.update
    let subscriptions = M.subscriptions
    let shutdown model = Tea_cmd.none
end

module MakeStandard ( M: StandardProgram ) = Make(OfStandard(M))

module OfBeginner ( M: BeginnerProgram ) : Program = struct
    type flags = unit
    type msg = M.msg
    type model = M.model
    let init () = ( M.init, Tea_cmd.none )
    let view = M.view
    let update model msg = ( M.update model msg , Tea_cmd.none )
    let subscriptions model = Tea_sub.none
    let shutdown model = Tea_cmd.none
end

module MakeBeginner (M : BeginnerProgram ) = Make(OfBeginner(M))

