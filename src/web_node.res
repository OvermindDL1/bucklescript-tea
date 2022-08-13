type style = {
  @get
  "setProperty": Js.undefined<Web_json.t> /* TODO:  Revamp this and the next line... */,
  @meth
  "setProperty__": (string, Js.null<string>, Js.null<string>) => unit,
}
@send external
setProperty__: (string, Js.null<string>, Js.null<string>) => unit="setProperty__"

@get_index external getStyle: (style, string) => Js.null<string> = ""

@set_index external setStyle: (style, string, Js.null<string>) => unit = ""

type rec t = {
  @get
  "style": style,
  @set @get
  "value": Js.undefined<string>,
  @set @get
  "checked": Js.undefined<bool>,
  @get
  "childNodes": Js.Array.t<t>,
  @get
  "firstChild": Js.Null.t<t>,
  /* Text Nodes only */
  @set @get({null: null})
  "nodeValue": string,
}

  
@send external
appendChild: (t,t) => t="appendChild"
@send external
removeChild: (t,t) => t="removeChild"
@send external
insertBefore: (t,t, t) => t="insertBefore"
@send external
remove: (t,unit) => unit="remove"
@send external
setAttributeNS: (t,string, string, string) => unit="setAttributeNS"
@send external
setAttribute: (t,string, string) => unit="setAttribute"
@send external
removeAttributeNS: (t,string, string) => unit="removeAttributeNS"
@send external
removeAttribute: (t,string) => unit="removeAttribute"
@send external
focus: t => unit="focus"

@send external
addEventListener: (t,string, Web_event.cb<t>, Web_event.options) => unit="addEventListener"

@send external
removeEventListener: (t,string, Web_event.cb<t>, Web_event.options) => unit="removeEventListener"



@val external document_node: t = "document"

type event = Web_event.t<t>

type event_cb = Web_event.cb<t>

@get_index external getProp_asEventListener: (t, 'key) => Js.undefined<Web_event.cb<t>> = ""

@set_index external setProp_asEventListener: (t, 'key, Js.undefined<Web_event.cb<t>>) => unit = ""

@get_index external getProp: (t, 'key) => 'value = ""

@set_index external setProp: (t, 'key, 'value) => unit = ""

let style = n => n["style"]

let getStyle = (n, key) => getStyle(n["style"], key)

let setStyle = (n, key, value) => setStyle(n["style"], key, value)

let setStyleProperty = (n, ~priority=false, key, value) => {
  let style = n["style"]
  switch Js.Undefined.toOption(style["setProperty"]) {
  | None => setStyle(n, key, value) /* TODO:  Change this to setAttribute sometime, maybe... */
  | Some(_valid) =>
    setProperty__(
      key,
      value,
      if priority {
        Js.Null.return("important")
      } else {
        Js.Null.empty
      },
    )
  }
}

let childNodes = n => n["childNodes"]

let firstChild = n => n["firstChild"]

let appendChild = (n, child) => appendChild(n,child)

let removeChild = (n, child) => removeChild(n,child)

let insertBefore = (n, child, refNode) => insertBefore(n,child, refNode)

let remove = (n, child) => remove(n,child)

let setAttributeNS = (n, namespace, key, value) => setAttributeNS(n,namespace, key, value)

let setAttribute = (n, key, value) => setAttribute(n,key, value)

let setAttributeNsOptional = (n, namespace, key, value) =>
  switch namespace {
  | "" => setAttribute(n,key, value)
  | ns => setAttributeNS(n,ns, key, value)
  }

let removeAttributeNS = (n, namespace, key) => removeAttributeNS(n,namespace, key)

let removeAttribute = (n, key) => removeAttribute(n,key)

let removeAttributeNsOptional = (n, namespace, key) =>
  switch namespace {
  | "" => removeAttribute(n,key)
  | ns => removeAttributeNS(n,ns, key)
  }

let addEventListener = (n, typ, listener, options) => addEventListener(n,typ, listener, options)

let removeEventListener = (n, typ, listener, options) =>
  removeEventListener(n,typ, listener, options)

let focus = n => focus(n)

/* Text Nodes only */

let set_nodeValue = (n, text) => n["nodeValue"] = text

let get_nodeValue = n => n["nodeValue"]

/* Polyfills */

let remove_polyfill: unit => unit = () =>
  %raw(`
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
  `)
