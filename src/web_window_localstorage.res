type t = {
  @get
  "length": int,
}

  @send external
  clear: t => unit="clear"
  @send external
  key: (t,int) => string="key"
  @send external
  getItem: (t,string) => string="getItem"
  @send external
  removeItem: (t,string) => unit="removeItem"
  @send external
  setItem: (t,string, string) => unit="setItem"

let length = window =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) => Some(localStorage["length"])
  }

let clear = window =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) => Some(clear(localStorage))
  }

let key = (window, idx) =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) => Some(key(localStorage,idx))
  }

let getItem = (window, key) =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) =>
    try Some(getItem(localStorage,key)) catch {
    | _ => None
    }
  }

let removeItem = (window, key) =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) => Some(removeItem(localStorage,key))
  }

let setItem = (window, key, value) =>
  switch Js.Undefined.toOption(window["localStorage"]) {
  | None => None
  | Some(localStorage) => Some(setItem(localStorage,key, value))
  }
