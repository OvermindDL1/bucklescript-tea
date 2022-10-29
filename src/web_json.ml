
let of_type (type a) (_v : a Js.Json.kind) (x : a) : Js.Json.t =
  Obj.magic x

let null : Js_types.null_val = Obj.magic Js.null
