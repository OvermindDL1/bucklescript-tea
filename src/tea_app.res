type program<'flags, 'model, 'msg> = {
  init: 'flags => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
}
type standardProgram<'flags, 'model, 'msg> = {
  init: 'flags => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
}
type beginnerProgram<'model, 'msg> = {
  model: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => Vdom.t<'msg>,
}
type pumpInterface<'model, 'msg> = {
  startup: unit => unit,
  render_string: 'model => string,
  handleMsg: ('model, 'msg) => 'model,
  shutdown: Tea_cmd.t<'msg> => unit,
}
type programInterface<'msg> = {
  "pushMsg": 'msg => unit,
  "shutdown": unit => unit,
  "getHtmlString": unit => string,
}
@obj
external makeProgramInterface: (
  ~pushMsg: 'msg => unit,
  ~shutdown: unit => unit,
  ~getHtmlString: unit => string,
) => programInterface<'msg> = ""
let programStateWrapper = (initModel, pump, shutdown) => {
  open Vdom
  let model = ref(initModel)
  let callbacks = ref({
    enqueue: _msg => Js.log("INVALID enqueue CALL!"),
    on: _ => (),
  })
  let pumperInterfaceC = () => pump(callbacks)
  let pumperInterface = pumperInterfaceC()
  let pending: ref<option<list<'msg>>> = (ref(None): ref<option<list<'msg>>>)
  let rec handler = msg =>
    switch pending.contents {
    | None =>
      let () = pending := Some(list{})
      let newModel = pumperInterface.handleMsg(model.contents, msg)
      let () = model := newModel
      switch pending.contents {
      | None =>
        failwith("INVALID message queue state, should never be None during message processing!")
      | Some(list{}) => pending := None
      | Some(msgs) =>
        let () = pending := None
        List.iter(handler, List.rev(msgs))
      }
    | Some(msgs) => pending := Some(list{msg, ...msgs})
    }
  let render_events = ref(list{})
  let finalizedCBs: Vdom.applicationCallbacks<'msg> = (
    {
      enqueue: msg => handler(msg),
      on: x =>
        switch x {
        | Render => List.iter(handler, render_events.contents)
        | AddRenderMsg(msg) => render_events := List.append(render_events.contents, list{msg})
        | RemoveRenderMsg(msg) =>
          render_events := List.filter(mg => msg !== mg, render_events.contents)
        },
    }: Vdom.applicationCallbacks<'msg>
  )
  let () = callbacks := finalizedCBs
  let pi_requestShutdown = () => {
    let () =
      callbacks := {
          enqueue: _msg => Js.log("INVALID message enqueued when shut down"),
          on: _ => (),
        }
    let cmd = shutdown(model.contents)
    let () = pumperInterface.shutdown(cmd)
  }
  let render_string = () => {
    let rendered = pumperInterface.render_string(model.contents)
    rendered
  }
  let () = pumperInterface.startup()
  makeProgramInterface(~pushMsg=handler, ~shutdown=pi_requestShutdown, ~getHtmlString=render_string)
}
let programLoop = (update, view, subscriptions, initModel, initCmd, x) =>
  switch x {
  | None =>
    callbacks => {
      let oldSub = ref(Tea_sub.none)
      let handleSubscriptionChange = model => {
        let newSub = subscriptions(model)
        oldSub := Tea_sub.run(callbacks, callbacks, oldSub.contents, newSub)
      }
      {
        startup: () => {
          let () = Tea_cmd.run(callbacks, initCmd)
          let () = handleSubscriptionChange(initModel)
        },
        render_string: model => {
          let vdom = view(model)
          let rendered = Vdom.renderToHtmlString(vdom)
          rendered
        },
        handleMsg: (model, msg) => {
          let (newModel, cmd) = update(model, msg)
          let () = Tea_cmd.run(callbacks, cmd)
          let () = handleSubscriptionChange(newModel)
          newModel
        },
        shutdown: cmd => {
          let () = Tea_cmd.run(callbacks, cmd)
          let () = oldSub := Tea_sub.run(callbacks, callbacks, oldSub.contents, Tea_sub.none)
        },
      }
    }
  | Some(parentNode) =>
    callbacks => {
      let priorRenderedVdom = ref(list{})
      let latestModel = ref(initModel)
      let nextFrameID = ref(None)
      let doRender = _delta =>
        switch nextFrameID.contents {
        | None => ()
        | Some(_id) =>
          let newVdom = list{view(latestModel.contents)}
          let justRenderedVdom = Vdom.patchVNodesIntoElement(
            callbacks,
            parentNode,
            priorRenderedVdom.contents,
            newVdom,
          )
          let () = priorRenderedVdom := justRenderedVdom
          let () = callbacks.contents.on(Render)
          nextFrameID := None
        }
      let scheduleRender = () =>
        switch nextFrameID.contents {
        | Some(_) => ()
        | None =>
          let realtimeRendering = false
          if realtimeRendering {
            let () = nextFrameID := Some(-1)
            doRender(16)
          } else {
            let id = Web.Window.requestAnimationFrame(doRender)
            let () = nextFrameID := Some(id)
          }
        }
      let clearPnode = () =>
        while Js.Array.length(Web.Node.childNodes(parentNode)) > 0 {
          switch Js.Null.toOption(Web.Node.firstChild(parentNode)) {
          | None => ()
          | Some(firstChild) =>
            let _removedChild = Web.Node.removeChild(parentNode, firstChild)
          }
        }
      let oldSub = ref(Tea_sub.none)
      let handleSubscriptionChange = model => {
        let newSub = subscriptions(model)
        oldSub := Tea_sub.run(callbacks, callbacks, oldSub.contents, newSub)
      }
      let handlerStartup = () => {
        let () = clearPnode()
        let () = Tea_cmd.run(callbacks, initCmd)
        let () = handleSubscriptionChange(latestModel.contents)
        let () = nextFrameID := Some(-1)
        let () = doRender(16)
      }
      let render_string = model => {
        let vdom = view(model)
        let rendered = Vdom.renderToHtmlString(vdom)
        rendered
      }
      let handler = (model, msg) => {
        let (newModel, cmd) = update(model, msg)
        let () = latestModel := newModel
        let () = Tea_cmd.run(callbacks, cmd)
        let () = scheduleRender()
        let () = handleSubscriptionChange(newModel)
        newModel
      }
      let handlerShutdown = cmd => {
        let () = nextFrameID := None
        let () = Tea_cmd.run(callbacks, cmd)
        let () = oldSub := Tea_sub.run(callbacks, callbacks, oldSub.contents, Tea_sub.none)
        let () = priorRenderedVdom := list{}
        let () = clearPnode()
      }
      {
        startup: handlerStartup,
        render_string: render_string,
        handleMsg: handler,
        shutdown: handlerShutdown,
      }
    }
  }
let program: (
  program<'flags, 'model, 'msg>,
  Js.null_undefined<Web.Node.t>,
  'flags,
) => programInterface<'msg> = (
  ({init, update, view, subscriptions, shutdown}, pnode, flags) => {
    let () = Web.polyfills()
    let (initModel, initCmd) = init(flags)
    let opnode = Js.Nullable.toOption(pnode)
    let pumpInterface = programLoop(update, view, subscriptions, initModel, initCmd, opnode)
    programStateWrapper(initModel, pumpInterface, shutdown)
  }: (
    program<'flags, 'model, 'msg>,
    Js.null_undefined<Web.Node.t>,
    'flags,
  ) => programInterface<'msg>
)
let standardProgram: (
  standardProgram<'flags, 'model, 'msg>,
  Js.null_undefined<Web.Node.t>,
  'flags,
) => programInterface<'msg> = (
  ({init, update, view, subscriptions}, pnode, args) =>
    program(
      {
        init: init,
        update: update,
        view: view,
        subscriptions: subscriptions,
        shutdown: _model => Tea_cmd.none,
      },
      pnode,
      args,
    ): (
    standardProgram<'flags, 'model, 'msg>,
    Js.null_undefined<Web.Node.t>,
    'flags,
  ) => programInterface<'msg>
)
let beginnerProgram: (
  beginnerProgram<'model, 'msg>,
  Js.null_undefined<Web.Node.t>,
  unit,
) => programInterface<'msg> = (
  ({model, update, view}, pnode, ()) =>
    standardProgram(
      {
        init: () => (model, Tea_cmd.none),
        update: (model, msg) => (update(model, msg), Tea_cmd.none),
        view: view,
        subscriptions: _model => Tea_sub.none,
      },
      pnode,
      (),
    ): (
    beginnerProgram<'model, 'msg>,
    Js.null_undefined<Web.Node.t>,
    unit,
  ) => programInterface<'msg>
)
let map = (func, vnode) => Vdom.map(func, vnode)
