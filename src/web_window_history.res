type t = {
  @get
  "length": int,
  @get
  "state": Js.Json.t,
}

@send external
back: t => unit="back"
@send external
forward: t => unit="forward"
@send external
go: (t,int) => unit="go"
@send external
pushState: (t,Js.Json.t, string, string) => unit="pushState"
@send external
replaceState: (t,Js.Json.t, string, string) => unit="replaceState"

let length = window =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => -1
  | Some(history) => history["length"]
  }

let back = window =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => ()
  | Some(history) => back(history)
  }

let forward = window =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => ()
  | Some(history) => forward(history)
  }

let go = (window, to') =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => ()
  | Some(history) => go(history,to')
  }

let pushState = (window, state, title, url) =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => ()
  | Some(history) => pushState(history,state, title, url)
  }

let replaceState = (window, state, title, url) =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => ()
  | Some(history) => replaceState(history,state, title, url)
  }

let state = window =>
  switch Js.Undefined.toOption(window["history"]) {
  | None => Js.Undefined.empty
  | Some(history) => history["state"]
  }
