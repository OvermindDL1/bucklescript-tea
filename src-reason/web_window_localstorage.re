type t = {
  .
  [@bs.get] "length": int,
  [@bs.meth] "clear": unit => unit,
  [@bs.meth] "key": int => string,
  [@bs.meth] "getItem": string => Js.Nullable.t(string),
  [@bs.meth] "removeItem": string => unit,
  [@bs.meth] "setItem": (string, string) => unit,
};

let localStorage = window =>
  try(Js.Undefined.toOption(window##localStorage)) {
  | _ => None
  };

let length = window =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) => Some(localStorage##length)
  };

let clear = window =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) => Some(localStorage##clear())
  };

let key = (window, idx) =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) => Some(localStorage##key(idx))
  };

let getItem = (window, key) =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) =>
    try(Some(localStorage##getItem(key))) {
    | _ => None
    }
  };

let removeItem = (window, key) =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) => Some(localStorage##removeItem(key))
  };

let setItem = (window, key, value) =>
  switch (localStorage(window)) {
  | None => None
  | Some(localStorage) => Some(localStorage##setItem(key, value))
  };
