include Js.Json;

type nothingYet;

[@bs.val]
external stringify : ('t, Js.null(nothingYet), int) => string =
  "JSON.stringify";

let string_of_json = (~indent=2, value) =>
  switch (Js.Undefined.toOption(value)) {
  | None => "undefined"
  | Some(v) =>
    try (stringify(v, Js.Null.empty, indent)) {
    | _ => ""
    }
  };

let of_type = (type a, _v: kind(a), x: a) : t => Obj.magic(x);

let null: Js_types.null_val = Obj.magic(Js.null);
