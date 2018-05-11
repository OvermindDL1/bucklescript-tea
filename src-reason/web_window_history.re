type t = {
  .
  [@bs.get] "length": int,
  [@bs.meth] "back": unit => unit,
  [@bs.meth] "forward": unit => unit,
  [@bs.meth] "go": int => unit,
  [@bs.meth] "pushState": (Js.Json.t, string, string) => unit,
  [@bs.meth] "replaceState": (Js.Json.t, string, string) => unit,
  [@bs.get] "state": Js.Json.t,
};

let length = window =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => (-1)
  | Some(history) => history##length
  };

let back = window =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => ()
  | Some(history) => history##back
  };

let forward = window =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => ()
  | Some(history) => history##forward
  };

let go = (window, to') =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => ()
  | Some(history) => history##go(to')
  };

let pushState = (window, state, title, url) =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => ()
  | Some(history) => history##pushState(state, title, url)
  };

let replaceState = (window, state, title, url) =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => ()
  | Some(history) => history##replaceState(state, title, url)
  };

let state = window =>
  switch (Js.Undefined.toOption(window##history)) {
  | None => Js.Undefined.empty
  | Some(history) => history##state
  };
