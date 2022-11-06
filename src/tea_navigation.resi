module Location: {
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
  let get: unit => t
}

type navigationProgram<'flags, 'model, 'msg> = {
  init: ('flags, Location.t) => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
}

let notifier: ref<option<Location.t => unit>>
let notifyUrlChange: unit => unit
let subscribe: (Location.t => 'a) => Tea_sub.t<'a>
let replaceState: string => unit
let pushState: string => unit
let modifyUrl: string => Tea_cmd.t<'a>
let newUrl: string => Tea_cmd.t<'a>
let go: int => Tea_cmd.t<'a>
let back: int => Tea_cmd.t<'a>
let forward: int => Tea_cmd.t<'a>

let navigationProgram: (
  Location.t => 'a,
  navigationProgram<'b, 'c, 'a>,
  Js.null_undefined<Dom.node>,
  'b,
) => Tea_app.programInterface<'a>
