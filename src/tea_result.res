/* TODO:  Remove this when Bucklescript is updated to OCaml 4.03 as it includes result */
type t /* result */<'a, 'b> = result<'a, 'b> =
  | Ok('a)
  | Error('b)

let result_to_option = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }

let option_of_result = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }

let ok = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }

let error = x =>
  switch x {
  | Ok(_) => None
  | Error(e) => Some(e)
  }

let rec last_of = x =>
  switch x {
  | list{} => failwith("`Tea.Result.do` must never be passed the empty list")
  | list{last} => last
  | list{next, ...tl} =>
    switch next {
    | Error(_) as e => e
    | Ok(_) => last_of(tl)
    }
  }

let rec accumulate = x =>
  switch x {
  | list{} => Ok(list{})
  | list{last} =>
    switch last {
    | Error(_) as e => e
    | Ok(o) => Ok(list{o})
    }
  | list{next, ...tl} =>
    switch next {
    | Error(_) as e => e
    | Ok(o) =>
      switch accumulate(tl) {
      | Error(_) as e => e
      | Ok(os) => Ok(list{o, ...os})
      }
    }
  }

let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }

let rec error_of_any = x =>
  switch x {
  | list{} => None
  | list{hd, ...tl} =>
    switch hd {
    | Error(e) => Some(e)
    | Ok(_) => error_of_any(tl)
    }
  }

let error_of_first = (fst, x) =>
  switch x {
  | Error(e) => Some(e)
  | Ok(_) => error(fst)
  }
