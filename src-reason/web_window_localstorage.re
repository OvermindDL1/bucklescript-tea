type t = {
  .
  [@bs.get] "length": int,
  [@bs.meth] "clear": unit => unit,
  [@bs.meth] "key": int => string,
  [@bs.meth] "getItem": string => string,
  [@bs.meth] "removeItem": string => unit,
  [@bs.meth] "setItem": (string, string) => unit,
};

let length = window =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) => Some(localStorage##length)
  };

let clear = window =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) => Some(localStorage##clear())
  };

let key = (window, idx) =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) => Some(localStorage##key(idx))
  };

let getItem = (window, key) =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) =>
    try (Some(localStorage##getItem(key))) {
    | _ => None
    }
  };

let removeItem = (window, key) =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) => Some(localStorage##removeItem(key))
  };

let setItem = (window, key, value) =>
  switch (Js.Undefined.toOption(window##localStorage)) {
  | None => None
  | Some(localStorage) => Some(localStorage##setItem(key, value))
  };
