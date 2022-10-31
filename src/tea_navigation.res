module Location = {
  type t = {
    href: string,
    protocol: string,
    host: string,
    hostname: string,
    port: string,
    pathname: string,
    search: string,
    hash: string,
    username: string,
    password: string,
    origin: string,
  }

  let get = (): t => {
    open Webapi.Dom
    open Location
    {
      href: href(location),
      protocol: protocol(location),
      host: host(location),
      hostname: hostname(location),
      port: port(location),
      pathname: pathname(location),
      search: search(location),
      hash: hash(location),
      username: username(location),
      password: password(location),
      origin: origin(location),
    }
  }
}

type navigationProgram<'flags, 'model, 'msg> = {
  init: ('flags, Location.t) => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
}

let notifier: ref<option<Location.t => unit>> = ref(None)

let notifyUrlChange = () =>
  switch notifier.contents {
  | None => ()
  | Some(cb) =>
    let location = Location.get()
    let () = cb(location)
  }

let subscribe = tagger => {
  open Vdom
  let enableCall = callbacks => {
    let notifyHandler = location => callbacks.enqueue(tagger(location))
    let () = notifier := Some(notifyHandler)
    let handler = _event => notifyUrlChange()
    let window = Webapi.Dom.window
    let () = Webapi.Dom.Window.addPopStateEventListener(window, handler)
    () => Webapi.Dom.Window.removePopStateEventListener(window, handler)
  }
  Tea_sub.registration("navigation", enableCall)
}

let replaceState = url => {
  let history = Webapi.Dom.history
  let state = Webapi.Dom.History.state(history)
  let _ = Webapi.Dom.History.replaceState(history, state, "", url)
}

let pushState = url => {
  let history = Webapi.Dom.history
  let state = Webapi.Dom.History.state(history)
  let _ = Webapi.Dom.History.pushState(history, state, "", url)
}

let modifyUrl = url =>
  Tea_cmd.call(_enqueue => {
    let () = replaceState(url)
    let () = notifyUrlChange()
  })

let newUrl = url =>
  Tea_cmd.call(_enqueue => {
    let () = pushState(url)
    let () = notifyUrlChange()
  })

let go = step =>
  Tea_cmd.call(_enqueue => {
    let history = Webapi.Dom.history
    let _ = Webapi.Dom.History.go(history, step)
    let () = notifyUrlChange()
  })

let back = step => go(-step)

let forward = step => go(step)

let navigationProgram = (locationToMessage, stuff) => {
  let init = flag => stuff.init(flag, Location.get())

  let subscriptions = model =>
    Tea_sub.batch(list{subscribe(locationToMessage), stuff.subscriptions(model)})

  open! Tea_app
  program({
    init: init,
    update: stuff.update,
    view: stuff.view,
    subscriptions: subscriptions,
    shutdown: stuff.shutdown,
  })
}
