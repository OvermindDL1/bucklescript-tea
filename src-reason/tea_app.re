/* "OAK-TEA" Maybe?  For OCaml Application Kernal TEA */

/* TODO:  Create a new program interface to make the program type just handle init/update/shutdown and such interface
   functionality.  Top level should just be a model change handler, what is currently 'view', and multiple programs
   that can update their own part of the model, of which the entirety can be accessed by view.  Probably should be in a
   Tea.AppEx package or something.  Unsure how to work with compatability with Tea.App, perhaps have Tea.App delegate to
   Tea.AppEx or so as a simple wrapper? */

/* module type Program = sig
     type flags
     type model
     type msg
     val init : flags -> model
     val update : model -> msg -> model * 'msg Tea_cmd.t
     val subscriptions : model -> int
     (* val view : model -> msg Vdom.t *)
   end */

/* type 'flags 'model testRec = {
     init : 'flags -> 'model
   } */

/* type ('flags, 'model, 'msg) fullProgram = {
     internal : unit -> unit;
     init : 'flags -> 'model * 'msg Tea_cmd.t;
     update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
     view : 'model -> 'msg Vdom.t;
   } */

type program('flags, 'model, 'msg) = {
  init: 'flags => ('model, Tea_cmd.t('msg)),
  update: ('model, 'msg) => ('model, Tea_cmd.t('msg)),
  view: 'model => Vdom.t('msg),
  renderCallback: 'model => unit,
  subscriptions: 'model => Tea_sub.t('msg),
  shutdown: 'model => Tea_cmd.t('msg),
};

type standardProgram('flags, 'model, 'msg) = {
  init: 'flags => ('model, Tea_cmd.t('msg)),
  update: ('model, 'msg) => ('model, Tea_cmd.t('msg)),
  view: 'model => Vdom.t('msg),
  renderCallback: 'model => unit,
  subscriptions: 'model => Tea_sub.t('msg),
};

type beginnerProgram('model, 'msg) = {
  model: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => Vdom.t('msg),
};

type pumpInterface('model, 'msg) = {
  startup: unit => unit,
  render_string: 'model => string,
  handleMsg: ('model, 'msg) => 'model,
  shutdown: Tea_cmd.t('msg) => unit,
};

type programInterface('msg) = {. "pushMsg": 'msg => unit};

[@bs.obj]
external makeProgramInterface:
  (
    ~pushMsg: 'msg => unit,
    ~shutdown: unit => unit,
    ~getHtmlString: unit => string
  ) =>
  programInterface('msg) =
  "";

/* TODO:  Need to refactor the program layers to layer everything properly, things are a bit mixed up right now... */

/* let programStateWrapper initModel pump =
   let model = ref initModel in
   let rec handler msg =
     let newModel = pump !model msg in
     (model := newModel) in
    handler */

let programStateWrapper = (initModel, pump, shutdown) => {
  /* let programStateWrapper : 'model -> ('msg Vdom.applicationCallbacks ref -> 'model -> 'msg) -> ('msg -> unit) = fun initModel pump -> */
  /* let programStateWrapper : 'model -> ('msg Vdom.applicationCallbacks ref -> 'model -> 'msg -> 'model) -> 'msg programInterface = fun initModel pump -> */
  open Vdom;
  let model = ref(initModel);
  let callbacks = ref({enqueue: _msg => Js.log("INVALID enqueue CALL!")});
  let pumperInterfaceC = () => pump(callbacks);
  let pumperInterface = pumperInterfaceC();
  /* let handler = function
     | None -> ()
     | Some msg ->
       let newModel = pumper !model msg in
       let () = (model := newModel) in
       () in */
  let pending: ref(option(list('msg))) = ref(None);
  let rec handler = msg =>
    switch (pending^) {
    | None =>
      let () = pending := Some([]);
      /* let () = Js.log ("APP", "mainloop", "pre", !model) in */
      let newModel = pumperInterface.handleMsg(model^, msg);
      /* let () = Js.log ("APP", "mainloop", "post", newModel) in */
      let () = model := newModel;
      switch (pending^) {
      | None =>
        failwith(
          "INVALID message queue state, should never be None during message processing!",
        )
      | Some([]) => pending := None
      | Some(msgs) =>
        let () = pending := None;
        List.iter(handler, List.rev(msgs));
      };
    | Some(msgs) => pending := Some([msg, ...msgs])
    };
  let finalizedCBs: Vdom.applicationCallbacks('msg) = {
    enqueue: msg => handler(msg),
  };
  let () = callbacks := finalizedCBs;
  let pi_requestShutdown = () => {
    let () =
      callbacks :=
        {enqueue: _msg => Js.log("INVALID message enqueued when shut down")};
    let cmd = shutdown(model^);
    let () = pumperInterface.shutdown(cmd);
    ();
  };
  let render_string = () => {
    let rendered = pumperInterface.render_string(model^);
    rendered;
  };
  let () = pumperInterface.startup();
  makeProgramInterface(
    ~pushMsg=handler,
    ~shutdown=pi_requestShutdown,
    ~getHtmlString=render_string,
  );
};

let programLoop =
    (update, view, renderCallback, subscriptions, initModel, initCmd) =>
  fun
  | None => (
      callbacks => {
        let oldSub = ref(Tea_sub.none);
        let handleSubscriptionChange = model => {
          /* let open Vdom in */
          let newSub = subscriptions(model);
          oldSub := Tea_sub.run(callbacks, callbacks, oldSub^, newSub);
        };
        {
          startup: () => {
            let () = Tea_cmd.run(callbacks, initCmd);
            let () = handleSubscriptionChange(initModel);
            ();
          },
          render_string: model => {
            let vdom = view(model);
            let rendered = Vdom.renderToHtmlString(vdom);
            rendered;
          },
          handleMsg: (model, msg) => {
            let (newModel, cmd) = update(model, msg);
            /* let open Vdom in */
            let () = Tea_cmd.run(callbacks, cmd);
            let () = handleSubscriptionChange(newModel);
            newModel;
          },
          shutdown: cmd => {
            let () = Tea_cmd.run(callbacks, cmd); /* TODO:  Perhaps add cancelable commands? */
            let () =
              oldSub :=
                Tea_sub.run(callbacks, callbacks, oldSub^, Tea_sub.none);
            ();
          },
        };
      }
    )
  | Some(parentNode) => (
      callbacks => {
        /* let priorRenderedVdom = ref [view initModel] in */
        let priorRenderedVdom = ref([]);
        /* let lastVdom = ref (!priorRenderedVdom) in */
        let latestModel = ref(initModel);
        let nextFrameID = ref(None);
        let doRender = _delta =>
          switch (nextFrameID^) {
          | None => () /* The render has been canceled, possibly by shutting down, do nothing */
          | Some(_id) =>
            let newVdom = [view(latestModel^)];
            let justRenderedVdom =
              Vdom.patchVNodesIntoElement(
                callbacks,
                parentNode,
                priorRenderedVdom^,
                newVdom,
              );
            let () = renderCallback(latestModel^);
            let () = priorRenderedVdom := justRenderedVdom;
            /* let () = Vdom.patchVNodesIntoElement callbacks parentNode !priorRenderedVdom !lastVdom in
               let () = priorRenderedVdom := (!lastVdom) in */
            nextFrameID := None;
          };
        let scheduleRender = () =>
          switch (nextFrameID^) {
          | Some(_) => () /* A frame is already scheduled, nothing to do */
          | None =>
            /* Use requestAnimationFrame unless we're trying to benchmark, in which case we use real-time rendering */
            let realtimeRendering = false;
            if (realtimeRendering) {
              let () = nextFrameID := Some(-1);
              doRender(16);
            } else {
              let id = Web.Window.requestAnimationFrame(doRender);
              let () = nextFrameID := Some(id);
              ();
            };
          };

        /* let () = Js.log (Vdom.createVNodeIntoElement callbacks !lastVdom parentNode) in */
        /* We own the passed in node, clear it out TODO:  Clear it out properly */
        /* let () = Js.log ("Blah", Web.Node.firstChild parentNode, Js.Null.test (Web.Node.firstChild parentNode), false, true) in */
        let clearPnode = () =>
          while (Js.Array.length(Web.Node.childNodes(parentNode)) > 0) {
            switch (Js.Null.toOption(Web.Node.firstChild(parentNode))) {
            | None => ()
            | Some(firstChild) =>
              let _removedChild =
                Web.Node.removeChild(parentNode, firstChild);
              ();
            };
          };
        /* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!lastVdom) in */
        /* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!priorRenderedVdom) in */
        /*  Initial render */
        let oldSub = ref(Tea_sub.none);
        let handleSubscriptionChange = model => {
          /* let open Vdom in */
          let newSub = subscriptions(model);
          oldSub := Tea_sub.run(callbacks, callbacks, oldSub^, newSub);
        };
        let handlerStartup = () => {
          let () = clearPnode();
          let () = Tea_cmd.run(callbacks, initCmd);
          let () = handleSubscriptionChange(latestModel^);
          let () = nextFrameID := Some(-1);
          let () = doRender(16);
          ();
        };
        let render_string = model => {
          let vdom = view(model);
          let rendered = Vdom.renderToHtmlString(vdom);
          rendered;
        };
        let handler = (model, msg) => {
          let (newModel, cmd) = update(model, msg);
          let () = latestModel := newModel;
          /* let open Vdom in */
          /* let () = Js.log ("APP", "latestModel", "precmd", !latestModel) in */
          let () = Tea_cmd.run(callbacks, cmd);
          /* let () = Js.log ("APP", "latestModel", "postcmd", !latestModel) in */
          /* TODO:  Figure out if it is better to get view on update like here, or do it in doRender... */
          /* let newVdom = view newModel in (* Process VDom diffs here with callbacks *) */
          /* let () = Vdom.patchVNodeIntoElement callbacks parentNode !lastVdom newVdom in */
          /* let () = Js.log lastVdom in */
          /* let () = Js.log newVdom in */
          /* let () = Js.log (Vdom.createVNodeIntoElement callbacks newVdom parentNode) in */
          /* let () = lastVdom := [newVdom] in */
          let () = scheduleRender();
          /* let () = Js.log ("APP", "latestModel", "presub", !latestModel) in */
          let () = handleSubscriptionChange(newModel);
          /* let () = Js.log ("APP", "latestModel", "postsub", !latestModel) in */
          newModel;
        };
        let handlerShutdown = cmd => {
          /* let open Vdom in */
          let () = nextFrameID := None;
          let () = Tea_cmd.run(callbacks, cmd);
          let () =
            oldSub := Tea_sub.run(callbacks, callbacks, oldSub^, Tea_sub.none);
          let () = priorRenderedVdom := [];
          let () = clearPnode();
          ();
        };
        {
          startup: handlerStartup,
          render_string,
          handleMsg: handler,
          shutdown: handlerShutdown,
        };
      }
    );

let program:
  (program('flags, 'model, 'msg), Js.null_undefined(Web.Node.t), 'flags) =>
  programInterface('msg) =
  (
    {init, update, view, renderCallback, subscriptions, shutdown},
    pnode,
    flags,
  ) => {
    let () = Web.polyfills();
    let (initModel, initCmd) = init(flags);
    let opnode = Js.Nullable.toOption(pnode);
    let pumpInterface =
      programLoop(
        update,
        view,
        renderCallback,
        subscriptions,
        initModel,
        initCmd,
        opnode,
      );
    programStateWrapper(initModel, pumpInterface, shutdown);
  };

let standardProgram:
  (
    standardProgram('flags, 'model, 'msg),
    Js.null_undefined(Web.Node.t),
    'flags
  ) =>
  programInterface('msg) =
  ({init, update, view, renderCallback, subscriptions}, pnode, args) =>
    program(
      {
        init,
        update,
        view,
        renderCallback,
        subscriptions,
        shutdown: _model => Tea_cmd.none,
      },
      pnode,
      args,
    );

let beginnerProgram:
  (beginnerProgram('model, 'msg), Js.null_undefined(Web.Node.t), unit) =>
  programInterface('msg) =
  ({model, update, view}, pnode, ()) =>
    standardProgram(
      {
        init: () => (model, Tea_cmd.none),
        update: (model, msg) => (update(model, msg), Tea_cmd.none),
        view,
        renderCallback: _ => (),
        subscriptions: _model => Tea_sub.none,
      },
      pnode,
      (),
    );

let map = (func, vnode) => Vdom.map(func, vnode);

/* let fullProgram program pnode flags =
   match Js.Nullable.toOption pnode with
   | None -> Web.Document.body ()
    | Some parentNode -> parentNode */

/* class fullProgramClass {internal; init; update; view} pnode flags = object(self) */
/* class ['msg, 'model] fullProgramClass
     (msgHandler : 'model -> 'msg -> 'model * 'msg Tea_cmd.t)
     (initModel : 'model)
     (initCmd : 'msg Tea_cmd.t)
     (view : 'model -> 'msg Vdom.t)
     pnode =
   object(self)
     val mutable model = initModel
     val mutable lastView = view initModel

     initializer
       Js.log initCmd

     method update (msg : 'msg) =
       let (newModel, newCmd) = msgHandler model msg in
       model <- newModel;
       cmd <- newCmd
   end */

/* let programStateWrapperInit initModel =
     ref initModel

   let programStateWrapper model pump =
     let rec handler msg =
       let newModel = pump handler !model msg in
       (model := newModel) in
     handler



   let programLoopInit msgHandler view model = function
     | None -> None
     | Some parentNode ->
       let vdom = view model in
       let () = Js.log (Vdom.createVNodesIntoElement msgHandler [vdom] parentNode) in
       let rvdom = ref vdom in
       Some (parentNode, rvdom)

   let programLoop = function
     | None -> fun update _view _initModel msgHandler model msg ->
       let newModel, _newCmd = update model msg in (* TODO:  Process commands to msgHandler *)
       newModel
     | Some (parentNode, lastVdom) -> fun update view initModel msgHandler ->
       let handler model msg =
         let newModel, _newCmd = update model msg in (* TODO:  Process commands to msgHandler *)
         let newVdom = view newModel in (* Process VDom diffs here with msgHandler *)
         (* let () = Js.log lastVdom in *)
         (* let () = Js.log newVdom in *)
         (lastVdom := newVdom);
         newModel in
       handler


   let program {init; update; view} pnode flags =
     let initModel, initCmd = init flags in
     let opnode = Js.Nullable.toOption pnode in
     let modelState = programStateWrapperInit initModel in
     let rec viewState msgHandler = programLoopInit msgHandler view initModel opnode
     and pump_unfixed msgHandler = programLoop viewState update view initModel msgHandler in
     (* let rec pump model msg = programLoop opnode update view initModel msgHandler model msg *)
     let rec msgHandler msg = programStateWrapper modelState (pump_unfixed msgHandler) msg in
     fun msg -> msgHandler msg */

/* new fullProgramClass
   update
   initModel
   initCmds
   view
   (Js.Nullable.toOption pnode) */

/* {
     internal = (fun () -> Js.log "internal update");
     init = init;
     update = update;
     view = view;
   } (Js.Nullable.toOption pnode) flags */

/* match Js.Nullable.toOption pnode with
   | None -> Web.Document.body ()
   | Some parentNode -> parentNode */

/* let beginnerProgram program = function
   | None -> Js.log 42
   | Some parentNode -> Js.log 84 */

/* let beginnerProgram program pnode = match Js.Nullable.toOption pnode with
   | None -> Web.Document.body ()
   | Some node -> node */

/* let beginnerPrograms pnode = match Js.Nullable.toOption pnode with
   | None -> Web.Document.body ()
   | Some node -> Web.Node.style node */

/*
 module type ProgramState = sig
 end

 module MakeProgram (Prog : Program) : ProgramState = struct
   (* module Program = Prog *)
 end

 let makeProgram p =
   let module P = (val p : Program) in
   (module struct
     let x = P.init
     let y = 42
   end : ProgramState)


 module type Main = sig
 end

 module type App = sig
 end

 (*
 module Make (Prog : Program) : App = struct
   (* let x = M.x + 1 *)
 end *)

 module Make (MainProg : Main) : App = struct
   (* let x = M.x + 1 *)
 end

 (* let programWithFlags (module Prog : Program) =
   42 *) */
