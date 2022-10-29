
let string_of_json ?(indent=2) value =
  match Js.Undefined.toOption value with
  | None -> "undefined"
  | Some v ->
    try Js.Json.stringifyWithSpace v indent
    with _ -> ""

let of_type (type a) (_v : a Js.Json.kind) (x : a) : Js.Json.t =
  Obj.magic x

let null : Js_types.null_val = Obj.magic Js.null
