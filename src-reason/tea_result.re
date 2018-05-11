/* TODO:  Remove this when Bucklescript is updated to OCaml 4.03 as it includes result */
type t /* result */('a, 'b) =
  | Ok('a)
  | Error('b);

let result_to_option =
  fun
  | Ok(a) => Some(a)
  | Error(_) => None;

let option_of_result =
  fun
  | Ok(a) => Some(a)
  | Error(_) => None;

let ok =
  fun
  | Ok(a) => Some(a)
  | Error(_) => None;

let error =
  fun
  | Ok(_) => None
  | Error(e) => Some(e);

let rec last_of =
  fun
  | [] => failwith("`Tea.Result.do` must never be passed the empty list")
  | [last] => last
  | [next, ...tl] =>
    switch (next) {
    | Error(_) as e => e
    | Ok(_) => last_of(tl)
    };

let rec accumulate =
  fun
  | [] => Ok([])
  | [last] =>
    switch (last) {
    | Error(_) as e => e
    | Ok(o) => Ok([o])
    }
  | [next, ...tl] =>
    switch (next) {
    | Error(_) as e => e
    | Ok(o) =>
      switch (accumulate(tl)) {
      | Error(_) as e => e
      | Ok(os) => Ok([o, ...os])
      }
    };

let first = fst =>
  fun
  | Error(_) as e => e
  | Ok(_) => fst;

let rec error_of_any =
  fun
  | [] => None
  | [hd, ...tl] =>
    switch (hd) {
    | Error(e) => Some(e)
    | Ok(_) => error_of_any(tl)
    };

let error_of_first = fst =>
  fun
  | Error(e) => Some(e)
  | Ok(_) => error(fst);
