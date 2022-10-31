module Decoder = {
  type error = String.t

  module ObjectDict = Belt.Map.String

  type t<'input, 'result> = Decoder('input => result<'result, error>)

  exception ParseFail(string)

  /* Primitive types */

  let string = Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONString(s) => Ok(s)
      | _ => Error("Non-string value")
      },
  )

  let int = Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONNumber(n) =>
        if n > float_of_int(min_int) && n < float_of_int(max_int) {
          Ok(int_of_float(n))
        } else {
          Error("number out of int range")
        }
      | _ => Error("Non-int value")
      },
  )

  let float = Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONNumber(n) => Ok(n)
      | _ => Error("Non-float-value")
      },
  )

  let bool = Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONTrue => Ok(true)
      | JSONFalse => Ok(false)
      | _ => Error("Non-boolean value")
      },
  )

  let null = v => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONNull => Ok(v)
      | _ => Error("Non-null value")
      },
  )

  /* Compound types */

  let list = (Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONArray(a) =>
        let parse = v =>
          switch decoder(v) {
          | Ok(r) => r
          | Error(e) => raise(ParseFail(e))
          }
        try Ok(Array.to_list(a) |> List.map(parse)) catch {
        | ParseFail(e) => Error("list -> " ++ e)
        }
      | _ => Error("Non-list value")
      },
  )

  let array = (Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONArray(a) =>
        let parse = v =>
          switch decoder(v) {
          | Ok(r) => r
          | Error(e) => raise(ParseFail(e))
          }
        try Ok(Array.map(parse, a)) catch {
        | ParseFail(e) => Error("array -> " ++ e)
        }
      | _ => Error("Non-array value")
      },
  )

  let keyValuePairs = (Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONObject(o) =>
        let keys = Js.Dict.keys(o)
        let parse = (k, l) =>
          switch Js.Dict.get(o, k) {
          | None => raise(ParseFail("Key is undefined: " ++ k))
          | Some(v) =>
            switch decoder(v) {
            | Ok(r) => list{(k, r), ...l}
            | Error(e) => raise(ParseFail(e))
            }
          }
        try Ok(Array.fold_right(parse, keys, list{})) catch {
        | ParseFail(e) => Error("Invalid keyValuePair parsing: " ++ e)
        }
      | _ => Error("Non-keyValuePair value")
      },
  )

  let dict = (Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONObject(o) =>
        let keys = Js.Dict.keys(o)
        let parse = (k, d) =>
          switch Js.Dict.get(o, k) {
          | None => raise(ParseFail("Key is undefined: " ++ k))
          | Some(v) =>
            switch decoder(v) {
            | Ok(r) => ObjectDict.set(d, k, r)
            | Error(e) => raise(ParseFail(e))
            }
          }
        let emptyDict = ObjectDict.empty
        try Ok(Array.fold_right(parse, keys, emptyDict)) catch {
        | ParseFail(e) => Error("Invalid dict parsing: " ++ e)
        }
      | _ => Error("Non-dict value")
      },
  )

  let field = (key, Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONObject(o) =>
        switch Js.Dict.get(o, key) {
        | None => Error("Field Value is undefined: " ++ key)
        | Some(v) =>
          switch decoder(v) {
          | Ok(_) as o => o
          | Error(e) => Error("field `" ++ (key ++ ("` -> " ++ e)))
          }
        }
      | _ => Error("Non-fieldable value")
      },
  )

  let at = (fields, dec) => List.fold_right(field, fields, dec)

  let index = (idx, Decoder(decoder)) => Decoder(
    value =>
      switch Js.Json.classify(value) {
      | JSONArray(a) =>
        if idx < 0 || idx > Array.length(a) {
          Error("Array index out of range: " ++ string_of_int(idx))
        } else {
          decoder(a[idx])
        }
      | _ => Error("Non-array value")
      },
  )

  let maybe = (Decoder(decoder)) => Decoder(
    value =>
      switch decoder(value) {
      | Ok(r) => Ok(Some(r))
      | Error(_) => Ok(None)
      },
  )

  let oneOf = decoders => Decoder(
    value => {
      let rec parse = (v, x) =>
        switch x {
        | list{} => Error("No one-of's matched")
        | list{Decoder(decoder), ...rest} =>
          try switch decoder(v) {
          | Ok(_) as ok => ok
          | Error(_) => parse(v, rest)
          } catch {
          | _ => parse(v, rest)
          }
        }
      parse(value, decoders)
    },
  )

  let map = (mapper, Decoder(decoder1)) => Decoder(
    value =>
      switch decoder1(value) {
      | Ok(v1) => Ok(mapper(v1))
      | Error(e) => Error("map " ++ e)
      },
  )

  let map2 = (mapper, Decoder(decoder1), Decoder(decoder2)) => Decoder(
    value =>
      switch (decoder1(value), decoder2(value)) {
      | (Ok(v1), Ok(v2)) => Ok(mapper(v1, v2))
      | (e1, e2) =>
        switch Tea_result.error_of_first(e1, e2) {
        | None => failwith("Impossible case")
        | Some(e) => Error("map2 -> " ++ e)
        }
      },
  )

  let map3 = (mapper, Decoder(decoder1), Decoder(decoder2), Decoder(decoder3)) => Decoder(
    value =>
      switch (decoder1(value), decoder2(value), decoder3(value)) {
      | (Ok(v1), Ok(v2), Ok(v3)) => Ok(mapper(v1, v2, v3))
      | (e1, e2, e3) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map3 -> " ++ e)
        }
      },
  )

  let map4 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
  ) => Decoder(
    value =>
      switch (decoder1(value), decoder2(value), decoder3(value), decoder4(value)) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4)) => Ok(mapper(v1, v2, v3, v4))
      | (e1, e2, e3, e4) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map4 -> " ++ e)
        }
      },
  )

  let map5 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
  ) => Decoder(
    value =>
      switch (decoder1(value), decoder2(value), decoder3(value), decoder4(value), decoder5(value)) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5)) => Ok(mapper(v1, v2, v3, v4, v5))
      | (e1, e2, e3, e4, e5) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map5 -> " ++ e)
        }
      },
  )

  let map6 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
  ) => Decoder(
    value =>
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6)) => Ok(mapper(v1, v2, v3, v4, v5, v6))
      | (e1, e2, e3, e4, e5, e6) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) |> first(e6) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map6 -> " ++ e)
        }
      },
  )

  let map7 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
    Decoder(decoder7),
  ) => Decoder(
    value =>
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
        decoder7(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6), Ok(v7)) =>
        Ok(mapper(v1, v2, v3, v4, v5, v6, v7))
      | (e1, e2, e3, e4, e5, e6, e7) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) |> first(e6) |> first(e7) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map7 -> " ++ e)
        }
      },
  )

  let map8 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
    Decoder(decoder7),
    Decoder(decoder8),
  ) => Decoder(
    value =>
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
        decoder7(value),
        decoder8(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6), Ok(v7), Ok(v8)) =>
        Ok(mapper(v1, v2, v3, v4, v5, v6, v7, v8))
      | (e1, e2, e3, e4, e5, e6, e7, e8) =>
        open! Tea_result
        switch e1
        |> first(e2)
        |> first(e3)
        |> first(e4)
        |> first(e5)
        |> first(e6)
        |> first(e7)
        |> first(e8) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map8 -> " ++ e)
        }
      },
  )

  /* Fancy Primitives */

  let succeed = v => Decoder(_value => Ok(v))

  let fail = e => Decoder(_value => Error(e))

  let value = Decoder(value => Ok(value))

  let andThen = (func, Decoder(decoder)) => Decoder(
    value =>
      switch decoder(value) {
      | Ok(r) =>
        let Decoder(andThenDecoder) = func(r)
        andThenDecoder(value)
      | Error(_) as err => err
      },
  )

  let lazy_ = func => andThen(func, succeed())

  let nullable = decoder => oneOf(list{null(None), map(v => Some(v), decoder)})

  /* Decoders */

  /* TODO:  Constrain this value type more */
  let decodeValue = (Decoder(decoder), value) =>
    try decoder(value) catch {
    | ParseFail(e) => Error(e)
    | _ => Error("Unknown JSON parsing error")
    }

  let decodeEvent = (Decoder(decoder), value: Dom.event) =>
    try decoder(Obj.magic(value)) catch {
    | ParseFail(e) => Error(e)
    | _ => Error("Unknown JSON parsing error")
    }

  let decodeString = (decoder, string) =>
    try {
      let value = Js.Json.parseExn(string)
      decodeValue(decoder, value)
    } catch {
    /* | JsException e -> Error ("Given an invalid JSON: " ^ e) */
    | _ => Error("Invalid JSON string")
    }
}

module Encoder = {
  type t = Js.Json.t

  let encode = (indentLevel: int, value: 'a) =>
    if Js.Undefined.testAny(value) {
      "undefined"
    } else {
      try Js.Json.stringifyWithSpace(Obj.magic(value), indentLevel) catch {
      | _ => ""
      }
    }

  /* Encoders */

  let string = (v: string): Js.Json.t => Obj.magic(v)

  let int = (v: int): Js.Json.t => Obj.magic(float_of_int(v))

  let float = (v: float): Js.Json.t => Obj.magic(v)

  let bool = (v: bool): Js.Json.t => Obj.magic(v)

  let null = Js.Json.null

  let object_ = (v): Js.Json.t => {
    let aux = (o, (k, v)) => {
      let () = Js.Dict.set(o, k, v)
      o
    }
    let o = List.fold_left(aux, Js.Dict.empty(), v)
    Obj.magic(o)
  }

  let array = (v: array<'t>): Js.Json.t => Obj.magic(Js.Json.Array, v)

  let list = (v: list<'t>): Js.Json.t => Obj.magic(Array.of_list(v))
}

type t = Js.Json.t
