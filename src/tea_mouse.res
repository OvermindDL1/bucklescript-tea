type position = {
  x: int,
  y: int,
}

let position = {
  open Tea_json.Decoder
  map2((x, y) => {x: x, y: y}, field("pageX", int), field("pageY", int))
}

let registerGlobal = (name, key, tagger) => {
  open Vdom
  let enableCall = callbacks_base => {
    let callbacks = ref(callbacks_base)
    let fn = ev => {
      open Tea_json.Decoder
      open Tea_result
      switch decodeEvent(position, ev) {
      | Error(_) => None
      | Ok(pos) => Some(tagger(pos))
      }
    }
    let handler = EventHandlerCallback(key, fn)
    let elem = Web_node.document_node
    let cache = eventHandler_Register(callbacks, elem, name, handler)
    () => {
      let _ = eventHandler_Unregister(elem, name, cache)
    }
  }
  Tea_sub.registration(key, enableCall)
}

let clicks = (~key="", tagger) => registerGlobal("click", key, tagger)

let moves = (~key="", tagger) => registerGlobal("mousemove", key, tagger)

let downs = (~key="", tagger) => registerGlobal("mousedown", key, tagger)

let ups = (~key="", tagger) => registerGlobal("mouseup", key, tagger)
