let result_to_option = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }

let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }

let error_of_first = (fst, x) =>
  switch x {
  | Error(e) => Some(e)
  | Ok(_) =>
    switch fst {
    | Ok(_) => None
    | Error(e) => Some(e)
    }
  }
