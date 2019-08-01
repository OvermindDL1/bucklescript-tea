type style = {
  .
  [@bs.get] "setProperty": Js.undefined(Web_json.t), /* TODO:  Revamp this and the next line... */
  [@bs.meth]
  "setProperty__": (string, Js.null(string), Js.null(string)) => unit,
};

type rect = {
  .
  "top": float,
  "bottom": float,
  "left": float,
  "right": float,
};

[@bs.get_index] external getStyle: (style, string) => Js.null(string) = "";

[@bs.set_index]
external setStyle: (style, string, Js.null(string)) => unit = "";

type t = {
  .
  [@bs.get] "style": style,
  [@bs.set] [@bs.get] "value": Js.undefined(string),
  [@bs.set] [@bs.get] "checked": Js.undefined(bool),
  [@bs.get] "childNodes": Js.Array.t(t),
  [@bs.get] "firstChild": Js.Null.t(t),
  [@bs.meth] "appendChild": t => t,
  [@bs.meth] "removeChild": t => t,
  [@bs.meth] "insertBefore": (t, t) => t,
  [@bs.meth] "remove": unit => unit,
  [@bs.meth] "setAttributeNS": (string, string, string) => unit,
  [@bs.meth] "setAttribute": (string, string) => unit,
  [@bs.meth] "removeAttributeNS": (string, string) => unit,
  [@bs.meth] "removeAttribute": string => unit,
  [@bs.meth]
  "addEventListener": (string, Web_event.cb(t), Web_event.options) => unit,
  [@bs.meth]
  "removeEventListener": (string, Web_event.cb(t), Web_event.options) => unit,
  [@bs.meth] "focus": unit => unit,
  [@bs.meth] "getBoundingClientRect": unit => rect,
  /* input and text area only */
  [@bs.meth] "setSelectionRange": (int, int) => unit,
  /* Text Nodes only */
  [@bs.set] [@bs.get {null: null}] "nodeValue": string,
};

[@bs.val] external document_node: t = "document";

type event = Web_event.t(t);

type event_cb = Web_event.cb(t);

[@bs.get_index]
external getProp_asEventListener: (t, 'key) => Js.undefined(Web_event.cb(t)) =
  "";

[@bs.set_index]
external setProp_asEventListener:
  (t, 'key, Js.undefined(Web_event.cb(t))) => unit =
  "";

[@bs.get_index] external getProp: (t, 'key) => 'value = "";

[@bs.set_index] external setProp: (t, 'key, 'value) => unit = "";

let style = n => n##style;

let getStyle = (n, key) => getStyle(n##style, key);

let setStyle = (n, key, value) => setStyle(n##style, key, value);

let setStyleProperty = (n, ~priority=false, key, value) => {
  let style = n##style;
  switch (Js.Undefined.toOption(style##setProperty)) {
  | None => setStyle(n, key, value) /* TODO:  Change this to setAttribute sometime, maybe... */
  | Some(_valid) =>
    style##setProperty__(
      key,
      value,
      if (priority) {
        Js.Null.return("important");
      } else {
        Js.Null.empty;
      },
    )
  };
};

let childNodes = n => n##childNodes;

let firstChild = n => n##firstChild;

let appendChild = (n, child) => n##appendChild(child);

let removeChild = (n, child) => n##removeChild(child);

let insertBefore = (n, child, refNode) => n##insertBefore(child, refNode);

let remove = (n, child) => n##remove(child);

let setAttributeNS = (n, namespace, key, value) =>
  n##setAttributeNS(namespace, key, value);

let setAttribute = (n, key, value) => n##setAttribute(key, value);

let setAttributeNsOptional = (n, namespace, key, value) =>
  switch (namespace) {
  | "" => n##setAttribute(key, value)
  | ns => n##setAttributeNS(ns, key, value)
  };

let removeAttributeNS = (n, namespace, key) =>
  n##removeAttributeNS(namespace, key);

let removeAttribute = (n, key) => n##removeAttribute(key);

let removeAttributeNsOptional = (n, namespace, key) =>
  switch (namespace) {
  | "" => n##removeAttribute(key)
  | ns => n##removeAttributeNS(ns, key)
  };

let addEventListener = (n, typ, listener, options) =>
  n##addEventListener(typ, listener, options);

let removeEventListener = (n, typ, listener, options) =>
  n##removeEventListener(typ, listener, options);

let focus = n => n##focus();

/* Text Nodes only */

let set_nodeValue = (n, text) => n##nodeValue #= text;

let get_nodeValue = n => n##nodeValue;

/* Polyfills */

let remove_polyfill: unit => unit =
  () => [%bs.raw
    {|
  // remove polyfill
  (function() {
    if (!('remove' in Element.prototype)) {
      Element.prototype.remove = function() {
        if (this.parentNode) {
          this.parentNode.removeChild(this);
        }
      };
    };
  }())
  |}
  ];
