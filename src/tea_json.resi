module Decoder: {
  type error = String.t
  module ObjectDict: {
    type rec key = String.t
    type rec t<'msg> = Belt.Map.String.t<'msg>
  }

  type t<'input, 'result> = Decoder('input => result<'result, error>)

  type exn += ParseFail(string)

  let string: t<Js.Json.t, string>

  let int: t<Js.Json.t, int>

  let float: t<Js.Json.t, float>

  let bool: t<Js.Json.t, bool>

  let null: 'msg => t<Js.Json.t, 'msg>

  let list: t<Js.Json.t, 'msg> => t<Js.Json.t, list<'msg>>

  let array: t<Js.Json.t, 'msg> => t<Js.Json.t, array<'msg>>

  let keyValuePairs: t<Js.Json.t, 'msg> => t<Js.Json.t, list<(Js.Dict.key, 'msg)>>

  let dict: t<Js.Json.t, 'msg> => t<Js.Json.t, ObjectDict.t<'msg>>

  let field: (Js.Dict.key, t<Js.Json.t, 'msg>) => t<Js.Json.t, 'msg>

  let at: (list<Js.Dict.key>, t<Js.Json.t, 'msg>) => t<Js.Json.t, 'msg>

  let index: (int, t<Js.Json.t, 'msg>) => t<Js.Json.t, 'msg>

  let maybe: t<'msg, 'b> => t<'msg, option<'b>>

  let oneOf: list<t<'msg, 'b>> => t<'msg, 'b>

  let map: ('msg => 'b, t<'c, 'msg>) => t<'c, 'b>

  let map2: (('msg, 'b) => 'c, t<'d, 'msg>, t<'d, 'b>) => t<'d, 'c>

  let map3: (('msg, 'b, 'c) => 'd, t<'e, 'msg>, t<'e, 'b>, t<'e, 'c>) => t<'e, 'd>

  let map4: (('msg, 'b, 'c, 'd) => 'e, t<'f, 'msg>, t<'f, 'b>, t<'f, 'c>, t<'f, 'd>) => t<'f, 'e>

  let map5: (
    ('msg, 'b, 'c, 'd, 'e) => 'f,
    t<'g, 'msg>,
    t<'g, 'b>,
    t<'g, 'c>,
    t<'g, 'd>,
    t<'g, 'e>,
  ) => t<'g, 'f>

  let map6: (
    ('msg, 'b, 'c, 'd, 'e, 'f) => 'g,
    t<'h, 'msg>,
    t<'h, 'b>,
    t<'h, 'c>,
    t<'h, 'd>,
    t<'h, 'e>,
    t<'h, 'f>,
  ) => t<'h, 'g>

  let map7: (
    ('msg, 'b, 'c, 'd, 'e, 'f, 'g) => 'h,
    t<'i, 'msg>,
    t<'i, 'b>,
    t<'i, 'c>,
    t<'i, 'd>,
    t<'i, 'e>,
    t<'i, 'f>,
    t<'i, 'g>,
  ) => t<'i, 'h>

  let map8: (
    ('msg, 'b, 'c, 'd, 'e, 'f, 'g, 'h) => 'i,
    t<'j, 'msg>,
    t<'j, 'b>,
    t<'j, 'c>,
    t<'j, 'd>,
    t<'j, 'e>,
    t<'j, 'f>,
    t<'j, 'g>,
    t<'j, 'h>,
  ) => t<'j, 'i>

  let succeed: 'msg => t<'b, 'msg>

  let fail: error => t<'msg, 'b>

  let value: t<'msg, 'msg>

  let andThen: ('msg => t<'b, 'c>, t<'b, 'msg>) => t<'b, 'c>

  let lazy_: (unit => t<'msg, 'b>) => t<'msg, 'b>

  let nullable: t<Js.Json.t, 'msg> => t<Js.Json.t, option<'msg>>

  let decodeValue: (t<'msg, 'b>, 'msg) => result<'b, error>

  let decodeEvent: (t<'msg, 'b>, Dom.event) => result<'b, error>

  let decodeString: (t<Js.Json.t, 'msg>, string) => result<'msg, error>
}

module Encoder: {
  type t = Js.Json.t
  let encode: (int, 'msg) => string
  let string: string => Js.Json.t
  let int: int => Js.Json.t
  let float: float => Js.Json.t
  let bool: bool => Js.Json.t
  let null: Js.Json.t
  let object_: list<(Js.Dict.key, Js.Json.t)> => Js.Json.t
  let array: array<Js.Json.t> => Js.Json.t
  let list: list<Js.Json.t> => Js.Json.t
}

type t = Js.Json.t
